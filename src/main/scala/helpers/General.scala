package helpers

import breeze.linalg._
import breeze.numerics._
import math.exp


object General {

  def assymetricalCoin(p: Double): Boolean = (math.random < p)
  
  def time(f: => Unit)={
    val s = System.currentTimeMillis
    f
    print("execution time in seconds: ")
    println((System.currentTimeMillis - s) * 1.0 / 1000.0)
  }
}