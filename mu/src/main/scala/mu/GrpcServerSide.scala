package mu

import io.grpc._
import io.grpc.MethodDescriptor.Marshaller

trait GrpcServerSide[S] {

  def (service: S).toServerServiceDefinition: ServerServiceDefinition

}

object GrpcServerSide {

  inline def derived[S]: GrpcServerSide[S] = ${ gen[S] }

  // This is a workaround for https://github.com/lampepfl/dotty/issues/9020
  def instance[S](f: S => ServerServiceDefinition): GrpcServerSide[S] =
    new GrpcServerSide[S] {
      def (service: S).toServerServiceDefinition: ServerServiceDefinition = f(service)
    }

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
         * TODO grab the encoding and other configuration from the @service annotation.
         * For the demo we just assume protobuf.
         */

        val serviceName = Expr(classSym.fullName)

        val methodDefinitions: List[Expr[S => ServerMethodDefinition[_, _]]] =
          classSym.classMethods.map { methodSym =>
            methodSym.tree match {
              case DefDef(methodName, tparams, paramss, returnTpt, rhs) if isValidMethod(tparams, paramss, rhs) =>
                val requestTpe = paramss.flatten.head.tpt.tpe
                val responseTpe = returnTpt.tpe

                (requestTpe.seal, responseTpe.seal) match {
                  case ('[$req], '[$resp]) =>
                    val serverCallHandler: Expr[S => ServerCallHandler[$req, $resp]] =
                      // TODO how can I construct this tree? Build an Apply by hand?
                      // (srv: Greeter) => unary[HelloRequest, HelloResponse](request => srv.SayHello(request)
                      '{ (srv: $s) =>
                        CallHandlers.unary[$req, $resp](request => {
                          println(request)
                          ???
                        })
                      }
                    println(serverCallHandler.show)

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

        val methodDefinitionsExpr: Expr[List[S => ServerMethodDefinition[_, _]]] =
          sequence(methodDefinitions)

        val result = '{
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
        println(result.show)
        result
      case _ =>
        qctx.error("Can only derive GrpcServerSide for @service-annotated traits or abstract classes")
        '{ ??? }
    }
  }


}

