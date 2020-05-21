package myapp

import mu._
import mu.Encoding.Protobuf

case class HelloRequest(name: String)

case class HelloResponse(greeting: String)

@service(encoding = Protobuf)
trait Greeter derives GrpcServerSide {

  def SayHello(req: HelloRequest): HelloResponse

}
