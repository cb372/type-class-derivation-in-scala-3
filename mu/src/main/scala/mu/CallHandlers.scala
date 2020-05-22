package mu

import io.grpc.ServerCallHandler
import io.grpc.stub.ServerCalls._
import io.grpc.stub.StreamObserver

object CallHandlers {

  def unary[Req, Resp](f: Req => Resp): ServerCallHandler[Req, Resp] = {
    asyncUnaryCall[Req, Resp](new UnaryMethod[Req, Resp] {
      def invoke(req: Req, responseObserver: StreamObserver[Resp]): Unit = {
        val response = f(req)
        responseObserver.onNext(response)
        responseObserver.onCompleted()
      }
    })
  }

}
