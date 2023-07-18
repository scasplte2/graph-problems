package rockthejvm.playground

import scala.io.Source

/**
 * This simple application is a place where you can try out any Scala code you like.
 * (not that you couldn't create your own, mind you.)
 *
 * Get creative and have fun!
 *
 * Daniel @ Rock the JVM
 */
object Playground extends App {
//  println("Rock the JVM!")
//  val fileContents = Source.fromFile("./")

  val configInfo =
    """{
        "car": {
          "requesters": {
            "request1": "key1",
            "request2": "key2"
          },
          "provider": {
            "request1": "key3",
            "request2": "key4"
          }
        },
        "truck": {
          "requesters": {
            "request1": "key2",
            "request2": "key3"
          },
          "provider": {
            "request1": "key2",
            "request2": "key3"
          }
        },
        "blah": {
          "requesters": {
            "request1": "key2"
          }
        }
      }"""


}
