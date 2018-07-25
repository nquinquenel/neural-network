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
import co.theasi.plotly._

object Runner {
  def main(args: Array[String]): Unit = {
    time {
      //digitsExample()
      distributedExample()
      //testGraph()
    } 
  }

  def testGraph(): Unit = {
    val xs = (0.0 to 2.0 by 0.1)
    val ys = xs.map { x => x*x }

    val plot = Plot().withScatter(xs, ys)

    draw(plot, "my-first-plot")
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
    val labelMap = HashMap[String, List[Double]](("Mesos", List(1.0)),
      ("Omega", List(0.0)))
    val classificationVectors: ArrayBuffer[List[Double]] = labels.map(l => labelMap(l))

    //Neural network
    val neuronsInLayers = List(2, 10, 1)
    val sigmoid = new SigmoidFunction(1)
    val gamma = 0.1
    val nn: NeuralNetwork = new FeedForwardNeuralNetwork(neuronsInLayers, sigmoid, gamma)

    val r = new Random()

    val pairs = r.shuffle(inputs.zip(classificationVectors).toSeq)

    //Split into training and testing sets
    val split = 30
    val (testing, training) = pairs.splitAt(split)

    var epoch = 0
    val tab_plot1_x = new ArrayBuffer[Double](136)
    val tab_plot1_y = new ArrayBuffer[Double](136)

    while(nn.getMaxDelta > 0.0001) {
      epoch += 1
      tab_plot1_x.clear()
      tab_plot1_y.clear()
      for(train <- training) {
        val plot_tmp = nn.train(train._1, train._2)

        tab_plot1_y += plot_tmp(0)
        tab_plot1_x += plot_tmp(1)
      }
    }

    println("Epoch number: " + epoch)

    println("Training done.")

    val tab_plot2_x = new ArrayBuffer[Double](30)
    val tab_plot2_y = new ArrayBuffer[Double](30)

    //Testing the neural network
    val results = for(test <- testing) yield {
      tab_plot2_y += nn.classify(test._1)(0)
      tab_plot2_x += test._2.map(sigmoid.customRound(_)).head.toDouble
      nn.classify(test._1).map(sigmoid.customRound(_)) == test._2.map(sigmoid.customRound(_))
    }

    println("Testing done.")

    val successCount = results.count(r => r)
    val percentage = BigDecimal(((successCount:Double)/(split:Double))*100).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble

    println(s"Upon testing the neural network got $successCount/$split results right -> $percentage%.")

    val points_x: ArrayBuffer[Double] = ArrayBuffer(0, 1)
    val points_y: ArrayBuffer[Double] = ArrayBuffer(0, 0)
    val total_y: ArrayBuffer[Double] = ArrayBuffer(0, 0)
    for (i <- Range(0, 30)) {
      if (tab_plot2_x(i) == 0) {
        total_y(0) += 1
        points_y(0) += tab_plot2_y(i)
      } else {
        total_y(1) += 1
        points_y(1) += tab_plot2_y(i)
      }
    }
    points_y(0) = points_y(0) / total_y(0)
    points_y(1) = points_y(1) / total_y(1)

    val training_plot = Plot()
      .withScatter(tab_plot1_x, tab_plot1_y, ScatterOptions()
        .mode(ScatterMode.Marker)
        .name("Data")
        .marker(
          MarkerOptions()
            .size(10)
            .color(152, 0, 0, 0.8)
            .lineWidth(2)
            .lineColor(0, 0, 0)))

    val testing_plot = Plot()
      .withScatter(tab_plot2_x, tab_plot2_y, ScatterOptions()
        .mode(ScatterMode.Marker)
        .name("Data")
        .marker(
          MarkerOptions()
            .size(10)
            .color(152, 0, 0, 0.8)
            .lineWidth(2)
            .lineColor(0, 0, 0)))
      .withScatter(points_x, points_y, ScatterOptions()
        .mode(ScatterMode.Line)
        .name("Fit"))
      .withScatter(ArrayBuffer(0, 1), ArrayBuffer(0, 1), ScatterOptions()
        .mode(ScatterMode.Line)
        .name("Y = T"))

    draw(training_plot, "training-plot", writer.FileOptions(overwrite=true))
    draw(testing_plot, "testing-plot", writer.FileOptions(overwrite=true))
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