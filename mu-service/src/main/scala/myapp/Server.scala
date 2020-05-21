package myapp

import io.grpc._

@main def runServer(): Unit = {
  val service = new Greeter {
    def SayHello(req: HelloRequest): HelloResponse =
      HelloResponse(s"Hi, ${req.name}!")
  }
  val serverServiceDef = service.toServerServiceDefinition
  val server = ServerBuilder.forPort(8080)
    .addService(serverServiceDef)
    .build()
    .start()
  println("Server started on port 8080")
  server.awaitTermination()
}
