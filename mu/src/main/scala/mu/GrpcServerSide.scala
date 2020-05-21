package mu

import io.grpc.ServerServiceDefinition

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

    typeOf[S].classSymbol match {
      case Some(classSym) if isTraitOrAbstractClass(classSym.flags) && hasServiceAnnotation(classSym.annots) =>
        /*
         * TODO grab the encoding and other configuration from the @service annotation.
         * For the demo we just assume protobuf.
         */

        classSym.classMethods.map { methodSym =>
          methodSym.tree match {
            case DefDef(name, tparams, paramss, returnTpt, rhs) if isValidMethod(tparams, paramss, rhs) =>
              println(name)
              println(paramss)
              println(returnTpt)
              /*
               * TODO for each method:
               * - summon a marshaller for the request and response
               * - build a MethodDefinition
               *
               */
          }
        }

        val result = '{
          GrpcServerSide.instance[$s]{ service =>
            ServerServiceDefinition.builder("class-name-goes-here").build()
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

