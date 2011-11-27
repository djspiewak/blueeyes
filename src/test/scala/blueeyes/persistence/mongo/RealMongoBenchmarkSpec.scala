package blueeyes.persistence.mongo

import blueeyes.json._
import blueeyes.json.JsonAST._
import blueeyes.concurrent.Future
import blueeyes.concurrent.Future._

import net.lag.configgy.Configgy

import akka.actor.Actor._
import akka.actor.Actor
import akka.routing.Routing._
import akka.dispatch.Dispatchers
import akka.util.Duration

import java.util.concurrent.TimeUnit
import org.scalacheck.Gen._
import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

class RealMongoBenchmarkSpec extends Specification with ArbitraryJValue with MongoImplicits with ScalaCheck{
  val testLive = (new java.io.File("/etc/default/blueeyes.conf")).exists
  if (testLive) Configgy.configure("/etc/default/blueeyes.conf")

  private lazy val mongo  = new RealMongo(Configgy.config.configMap("mongo"))
  private lazy val database  = mongo.database( "mydb" )

  private val collection    = "test-collection"

  override def is = args(skipAll = true) ^ super.is

  "Mongo" should {
    "insert multiple quiries concurrently" in{
      val start = System.currentTimeMillis

      val actors = List.range(1, 50) map {index =>
        val actor = Actor.actorOf(new MessageActor(alphaStr.sample.get, alphaStr.sample.get, index))
        actor.start()
        actor
      }
      val futures = Future(actors.map(actor => fromAkka[(Future[Option[JObject]], String, String)](actor.?("send", 2000).mapTo[Tuple3[Future[Option[JObject]], String, String]]).toBlueEyes): _*)
      futures.value must eventually (beSome)

      futures.value.get.foreach{ v =>
        val (selectFuture, name, value) = v
        selectFuture.value must eventually (beSome)
        selectFuture.value.get.get \ name mustEqual(JString(value))
      }

      val removeFuture = database(remove.from(collection))
      val result = removeFuture.value must eventually (beSome)

      println("Execution time = " + (System.currentTimeMillis - start))

      result
    }
  }

  private val testDispatcher = Dispatchers.newExecutorBasedEventDrivenDispatcher("test")
      .withNewThreadPoolWithLinkedBlockingQueueWithUnboundedCapacity.setCorePoolSize(50)
      .setMaxPoolSize(100).setKeepAliveTime(Duration(30, TimeUnit.SECONDS)).build

  class MessageActor(name: String, value: String, index: Int) extends Actor{
    self.dispatcher = testDispatcher

    def receive = {
      case "send" => {
        val filter = "bucketId" === index && "entityId"    === index && "variationId" === index

        val insertFuture = database(upsert(collection).set(name set value).where(filter))
        val selectFuture = insertFuture.flatMap{v => database(selectOne().from(collection).where(filter))}

        self.reply((selectFuture, name, value))
      }
      case _ =>
    }
  }
}

