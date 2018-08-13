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
import org.apache.poi.ss.usermodel.{DataFormatter, Row, WorkbookFactory}
import java.io.File

import breeze.linalg.{DenseMatrix, DenseVector}

import collection.JavaConversions._
import scala.collection.mutable.{ArrayBuffer, Buffer}
import co.theasi.plotly._

import scala.collection.mutable

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

    val f = new File("example_data/Training-set-v2.xlsx")
    val workbook = WorkbookFactory.create(f)
    val sheet = workbook.getSheetAt(0)

    var inputs = new ArrayBuffer[ArrayBuffer[Double]]
    var labels = new ArrayBuffer[String]

    for (row <- sheet) {
      if (row.getRowNum() > 1) {
        var tmpArray = new ArrayBuffer[Double]()
        tmpArray += row.getCell(0).getStringCellValue.toDouble
        tmpArray += row.getCell(1).getStringCellValue.toDouble
        inputs += tmpArray

        // If yellow then Mesos is better
        // Else Omega is better
        if (row.getCell(15).getNumericCellValue == 0) {
          labels += "Mesos"
        } else {
          labels += "Omega"
        }
      }
    }

    println("Reading done.")

    val split = 70

    /*
     * Plots variables
     */

    val activate_plots = false

    val tab_plot1_x = new ArrayBuffer[Double](inputs.size - split)
    val tab_plot1_y = new ArrayBuffer[Double](inputs.size - split)

    val points1_x: ArrayBuffer[Double] = ArrayBuffer(0, 1)
    val points1_y: ArrayBuffer[Double] = ArrayBuffer(0, 0)
    val total1_y: ArrayBuffer[Double] = ArrayBuffer(0, 0)

    val points2_x: ArrayBuffer[Double] = ArrayBuffer(0, 1)
    val points2_y: ArrayBuffer[Double] = ArrayBuffer(0, 0)
    val total2_y: ArrayBuffer[Double] = ArrayBuffer(0, 0)

    val tab_plot2_x = new ArrayBuffer[Double](split)
    val tab_plot2_y = new ArrayBuffer[Double](split)

    //

    val validation_data = new ArrayBuffer[Double](split)
    var validation_weights = new ArrayBuffer[DenseMatrix[Double]](1)
    var validation_highest = 0

    for (idx <- Range(0, 1).inclusive) {
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
    val (testing, trainingtmp) = pairs.splitAt(split)

    val (validating, training) = trainingtmp.splitAt(split)

    var epoch = 0
    var epochStop = 0

    while(nn.getMaxDelta > 0.000001 && epoch != 3000) {
    //while(epoch != maxEpoch) {
      epoch += 1
      if (activate_plots) {
        tab_plot1_x.clear()
        tab_plot1_y.clear()
      }
      for(train <- training) {
        val plot_tmp = nn.train(train._1, train._2)

        if (activate_plots) {
          tab_plot1_y += plot_tmp(0)
          tab_plot1_x += plot_tmp(1)
        }
      }

      val tmp_results = for(validate <- validating) yield {
        nn.classify(validate._1).map(sigmoid.customRound(_)) == validate._2.map(sigmoid.customRound(_))
      }

      var validation_tmp_result = tmp_results.count(r => r)
      if (validation_highest <= validation_tmp_result) {
        epochStop = epoch
        validation_weights = nn.getWeights()
        validation_highest = validation_tmp_result
      }
      validation_data += validation_tmp_result
    }

    if (activate_plots) {
      for (i <- Range(0, inputs.size - split)) {
        if (tab_plot1_x(i) == 0) {
          total1_y(0) += 1
          points1_y(0) += tab_plot1_y(i)
        } else {
          total1_y(1) += 1
          points1_y(1) += tab_plot1_y(i)
        }
      }
      points1_y(0) = points1_y(0) / total1_y(0)
      points1_y(1) = points1_y(1) / total1_y(1)
    }

    println("Epoch number in total: " + epoch + " (max is 3000)")
    println("Best result on validation set : " + validation_highest + "/" + split + " on epoch n°" + epochStop)

    println("Training done.")

    nn.setWeights(validation_weights)

    //Testing the neural network
    var results = for(test <- testing) yield {
      if (activate_plots) {
        tab_plot2_y += nn.classify(test._1)(0)
        tab_plot2_x += test._2.map(sigmoid.customRound(_)).head.toDouble
      }
      nn.classify(test._1).map(sigmoid.customRound(_)) == test._2.map(sigmoid.customRound(_))
    }

    println("Testing done.")

    var successCount = results.count(r => r)
    var percentage = BigDecimal(((successCount:Double)/(split:Double))*100).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble

    println(s"Upon testing the neural network got $successCount/$split results right -> $percentage%.")

    if (activate_plots) {
      for (i <- Range(0, 30)) {
        if (tab_plot2_x(i) == 0) {
          total2_y(0) += 1
          points2_y(0) += tab_plot2_y(i)
        } else {
          total2_y(1) += 1
          points2_y(1) += tab_plot2_y(i)
        }
      }
      points2_y(0) = points2_y(0) / total2_y(0)
      points2_y(1) = points2_y(1) / total2_y(1)

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
        .withScatter(points1_x, points1_y, ScatterOptions()
          .mode(ScatterMode.Line)
          .name("Fit"))
        .withScatter(ArrayBuffer(0, 1), ArrayBuffer(0, 1), ScatterOptions()
          .mode(ScatterMode.Line)
          .name("Y = T"))

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
        .withScatter(points2_x, points2_y, ScatterOptions()
          .mode(ScatterMode.Line)
          .name("Fit"))
        .withScatter(ArrayBuffer(0, 1), ArrayBuffer(0, 1), ScatterOptions()
          .mode(ScatterMode.Line)
          .name("Y = T"))

      val distributed_points = (0 until 136)
      val tab_plot1_y_error = new ArrayBuffer[Double](136)

      for (i <- Range(0, 136)) {
        tab_plot1_y_error += tab_plot1_x(i) - tab_plot1_y(i)
      }

      val training_plot_error = Plot()
        .withScatter(distributed_points, tab_plot1_x, ScatterOptions()
          .mode(ScatterMode.Line)
          .name("Expected"))
        .withScatter(distributed_points, tab_plot1_y, ScatterOptions()
          .mode(ScatterMode.Line)
          .name("Reality"))

      val distributed_points2 = (0 until 30)
      val tab_plot2_y_error = new ArrayBuffer[Double](136)

      for (i <- Range(0, 30)) {
        tab_plot2_y_error += tab_plot2_x(i) - tab_plot2_y(i)
      }

      val testing_plot_error = Plot()
        .withScatter(distributed_points, tab_plot2_x, ScatterOptions()
          .mode(ScatterMode.Line)
          .name("Expected"))
        .withScatter(distributed_points, tab_plot2_y, ScatterOptions()
          .mode(ScatterMode.Line)
          .name("Reality"))

      draw(training_plot, "training-plot", writer.FileOptions(overwrite = true))
      draw(testing_plot, "testing-plot", writer.FileOptions(overwrite = true))
      draw(training_plot_error, "training-plot-error", writer.FileOptions(overwrite = true))
      draw(testing_plot_error, "testing-plot-error", writer.FileOptions(overwrite = true))
    }
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