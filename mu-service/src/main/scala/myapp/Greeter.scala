package myapp

import mu._
import mu.Encoding.Protobuf

case class HelloRequest(name: String)

case class HelloResponse(greeting: String)

@service(encoding = Protobuf)
trait Greeter derives GrpcServerSide {

  def SayHello(req: HelloRequest): HelloResponse

}

object Greeter {

  // Usually these marshallers would be auto-derived by Mu using PBDirect
  // but that doesn't exist for Dotty yet

  import java.io._

  given as ProtobufMarshaller[HelloRequest] {
    def stream(value: HelloRequest): InputStream = {
      val bytes = Array(10.toByte, value.name.size.toByte) ++ value.name.getBytes
      new ByteArrayInputStream(bytes)
    }
    def parse(stream: InputStream): HelloRequest = {
      val in = new DataInputStream(stream)
      in.read()
      val size = in.read().toInt
      val bytes = Array.ofDim[Byte](size)
      in.readFully(bytes)
      val name = new String(bytes)
      HelloRequest(name)
    }
  }

  given as ProtobufMarshaller[HelloResponse] {
    def stream(value: HelloResponse): InputStream = {
      val bytes = Array(10.toByte, value.greeting.size.toByte) ++ value.greeting.getBytes
      new ByteArrayInputStream(bytes)
    }
    def parse(stream: InputStream): HelloResponse = {
      val in = new DataInputStream(stream)
      in.read()
      val size = in.read().toInt
      val bytes = Array.ofDim[Byte](size)
      in.readFully(bytes)
      val greeting = new String(bytes)
      HelloResponse(greeting)
    }
  }
}
