package mu

import io.grpc._
import io.grpc.MethodDescriptor.Marshaller

trait GrpcServerSide[S] {

  def (service: S).toServerServiceDefinition: ServerServiceDefinition

}

object GrpcServerSide {

  inline def derived[S]: GrpcServerSide[S] = ${ gen[S] }

  import scala.quoted._
  import scala.reflect._

  def gen[S](using qctx: QuoteContext, s: Type[S]): Expr[GrpcServerSide[S]] = {
    import qctx.tasty._

    def isTraitOrAbstractClass(flags: Flags) =
      flags.is(Flags.Trait) || flags.is(Flags.Abstract)

    def hasServiceAnnotation(annots: List[Term]) =
      annots.exists(_.tpe <:< typeOf[mu.service])

    def isValidMethod(tparams: List[TypeDef], paramss: List[List[ValDef]], rhs: Option[Term]) =
      tparams.isEmpty && paramss.flatten.size == 1 && rhs.isEmpty

    def sequence(exprs: List[Expr[S => ServerMethodDefinition[_, _]]]): Expr[List[S => ServerMethodDefinition[_, _]]] = exprs match {
      case h :: t => '{ $h :: ${sequence(t)} }
      case Nil => '{ Nil: List[S => ServerMethodDefinition[_, _]] }
    }

    typeOf[S].classSymbol match {
      case Some(classSym) if isTraitOrAbstractClass(classSym.flags) && hasServiceAnnotation(classSym.annots) =>
        /*
         * In the real Mu we would grab the encoding and other configuration
         * from the @service annotation.
         * For the demo we just assume protobuf.
         */

        val serviceName = Expr(classSym.fullName)

        /*
         * For each method in the trait, we build a gRPC method definition:
         *   - construct a ServerCallHandler that, given an instance of the service trait
         *     and a request, calls the method on the instance to get the response
         *   - Summon marshallers for the method's request and response types
         *   - generate the appropriate fully-qualified name ("package.serviceName/methodName")
         */
        val methodDefinitions: List[Expr[S => ServerMethodDefinition[_, _]]] =
          classSym.classMethods.map { methodSym =>
            methodSym.tree match {
              case DefDef(methodName, tparams, paramss, returnTpt, rhs)
                if isValidMethod(tparams, paramss, rhs) =>

                val requestTpe = paramss.flatten.head.tpt.tpe
                val responseTpe = returnTpt.tpe

                (requestTpe.seal, responseTpe.seal) match {
                  case ('[$req], '[$resp]) =>
                    val serverCallHandler: Expr[S => ServerCallHandler[$req, $resp]] =
                      '{ (srv: $s) =>
                        CallHandlers.unary[$req, $resp](request =>
                          ${
                            Apply(
                              Select('srv.unseal, methodSym),
                              List('request.unseal)
                            ).seal.cast
                          }
                        )
                      }

                    val requestMarshaller: Expr[Marshaller[$req]] =
                      Expr.summon[ProtobufMarshaller[$req]] match {
                        case Some(marshaller) =>
                          marshaller
                        case None =>
                          qctx.error(s"Could not summon a Protobuf marshaller for request type $req")
                          '{ ??? }
                      }

                    val responseMarshaller: Expr[Marshaller[$resp]] =
                      Expr.summon[ProtobufMarshaller[$resp]] match {
                        case Some(marshaller) =>
                          marshaller
                        case None =>
                          qctx.error(s"Could not summon a Protobuf marshaller for response type $resp")
                          '{ ??? }
                      }

                    val fullMethodName = Expr(s"${classSym.fullName}/$methodName")

                    val methodDescriptor: Expr[MethodDescriptor[$req, $resp]] = '{
                      MethodDescriptor
                        .newBuilder($requestMarshaller, $responseMarshaller)
                        .setType(MethodDescriptor.MethodType.UNARY)
                        .setFullMethodName($fullMethodName)
                        .build()
                    }

                    '{ (srv: $s) =>
                      ServerMethodDefinition.create($methodDescriptor, $serverCallHandler(srv))
                    }
                }
            }
          }

        // Turn a list of expressions into an expression of a list
        val methodDefinitionsExpr: Expr[List[S => ServerMethodDefinition[_, _]]] =
          sequence(methodDefinitions)

        '{
          // this line is a workaround for https://github.com/lampepfl/dotty/issues/9020
          type X = $s
          new GrpcServerSide[X]{
            def (service: S).toServerServiceDefinition: ServerServiceDefinition = {
              val builder = ServerServiceDefinition.builder($serviceName)
              ${methodDefinitionsExpr}.foreach { f =>
                builder.addMethod(f(service))
              }
              builder.build()
            }
          }
        }
      case _ =>
        qctx.error("Can only derive GrpcServerSide for @service-annotated traits or abstract classes")
        '{ ??? }
    }
  }


}

