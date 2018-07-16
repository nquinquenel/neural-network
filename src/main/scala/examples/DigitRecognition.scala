package examples

import java.lang.System.out.println

import scala.Array.canBuildFrom
import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION
import scala.collection.immutable.HashMap
import scala.io.Source.fromFile
import scala.util.Random

import Helpers.time
import net.almost_done.nn.FeedForwardNeuralNetwork
import net.almost_done.nn.NeuralNetwork
import net.almost_done.nn.SigmoidFunction

import org.apache.poi.ss.usermodel.{ DataFormatter, WorkbookFactory, Row }
import java.io.File
import collection.JavaConversions._ // lets you iterate over a java iterable
import scala.collection.mutable.ArrayBuffer

object Runner {
  def main(args: Array[String]): Unit = {
    time {
      //digitsExample()
      distributedExample()
    } 
  }

  def distributedExample(): Unit = {

    println("Starting.")

    val f = new File("example_data/Training-set.xls")
    val workbook = WorkbookFactory.create(f)
    val sheet = workbook.getSheetAt(0)

    var inputs = new ArrayBuffer[ArrayBuffer[Double]]
    var labels = new ArrayBuffer[String]

    for (row <- sheet) {
      if (row.getRowNum() > 2) {
        var tmpArray = new ArrayBuffer[Double]()
        tmpArray += row.getCell(0).getStringCellValue.toDouble
        tmpArray += row.getCell(1).getStringCellValue.toDouble
        inputs += tmpArray

        // If yellow then Mesos is better
        // Else Omega is better
        if (row.getCell(0).getCellStyle.getFillBackgroundColor == 8) {
          labels += "Mesos"
        } else {
          labels += "Omega"
        }
      }
    }

    println("Reading done.")

    for(idx <- Range(0, 1).inclusive) {
      val min = inputs.map(characteristic => characteristic(idx)).min
      val max = inputs.map(characteristic => characteristic(idx)).max

      val spread = max - min
      if (spread != 0) {
        inputs.map(arr => arr(idx) = ((arr(idx) - min) / spread))
      }
    }

    println("Normalization done.")

    //Convert labels to 1-0 vectors
    val labelMap = HashMap[String, List[Double]](("Mesos", List(1.0, 0.0)),
      ("Omega", List(0.0, 1.0)))
    val classificationVectors: ArrayBuffer[List[Double]] = labels.map(l => labelMap(l))

    //Neural network
    val neuronsInLayers = List(2, 15, 2)
    val sigmoid = new SigmoidFunction(0.55)
    val gamma = 0.3
    val nn: NeuralNetwork = new FeedForwardNeuralNetwork(neuronsInLayers, sigmoid, gamma)

    val r = new Random()

    val pairs = r.shuffle(inputs.zip(classificationVectors).toSeq)

    //Split into training and testing sets
    val split = 30
    val (testing, training) = pairs.splitAt(split)

    while(nn.getMaxDelta > 0.0001) for(train <- training) {
      nn.train(train._1, train._2)
    }

    println("Training done.")

    //Testing the neural network
    val results = for(test <- testing) yield {
      nn.classify(test._1).map(sigmoid.customRound(_)) == test._2.map(sigmoid.customRound(_))
    }

    println("Testing done.")

    val successCount = results.count(r => r)
    val percentage = BigDecimal(((successCount:Double)/(split:Double))*100).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble

    println(s"Upon testing the neural network got $successCount/$split results right -> $percentage%.")
  }

  def digitsExample(): Unit = {

    println("Starting.")

    //Reading MNIST data
    val lines = fromFile("example_data/train.csv").getLines.drop(1).toArray.map(_.split(","))
    def getPixels(sr: Array[String]): Array[Double] = {
        assert(sr.size == 785)
        var ret:Array[Double] = new Array[Double](784)
        for (i <- 1 to 784) ret(i-1) = sr(i).toDouble
        ret
    }

    var coords: Array[Array[Double]] = lines.map(getPixels(_))
    val labels: Array[String] = lines.map(l => l(0))

    println("Reading done.")

    //Normalizing the data
    for(idx <- Range(0, 783).inclusive) {
      val min = coords.map(characteristic => characteristic(idx)).min
      val max = coords.map(characteristic => characteristic(idx)).max

      val spread = max - min
      if (spread != 0) {
        coords.map(arr => arr(idx) = ((arr(idx) - min) / spread))
      }
    }

    println("Normalization done.")

    //Convert labels to 1-0 vectors
    val labelMap = HashMap[String, List[Double]](("0", List(1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)),
                    ("1", List(0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)),
                    ("2", List(0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)),
                    ("3", List(0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)),
                    ("4", List(0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0)),
                    ("5", List(0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0)),
                    ("6", List(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0)),
                    ("7", List(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0)),
                    ("8", List(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0)),
                    ("9", List(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0)))
    val classificationVectors: Array[List[Double]] = labels.map(l => labelMap(l))

    //Neural network
    val neuronsInLayers = List(784, 30, 10)
    val sigmoid = new SigmoidFunction(0.55)
    val gamma = 0.3
    val nn: NeuralNetwork = new FeedForwardNeuralNetwork(neuronsInLayers, sigmoid, gamma)

    val r = new Random()

    val pairs = r.shuffle(coords.zip(classificationVectors).toSeq)

    //Split into training and testing sets
    val split = 5000
    val (testing, training) = pairs.splitAt(split)

    while(nn.getMaxDelta > 0.0001) for(train <- training) {
      nn.train(train._1, train._2)
    }

    println("Training done.")

    //Testing the neural network
    val results = for(test <- testing) yield {
      nn.classify(test._1).map(sigmoid.customRound(_)) == test._2.map(sigmoid.customRound(_))
    }

    println("Testing done.")

    val successCount = results.count(r => r)
    val percentage = BigDecimal(((successCount:Double)/(split:Double))*100).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble

    println(s"Upon testing the neural network got $successCount/$split results right -> $percentage%.")

  }

} 