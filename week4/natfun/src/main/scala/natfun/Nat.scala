package natfun

import java.util.NoSuchElementException

/**
  * Created by crusk on 3/28/16.
  */
abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  def isZero: Boolean = true
  def predecessor: Nat = throw NoSuchElementException
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat = that
  def - (that: Nat): Nat = if (that.isZero) this else throw NoSuchElementException
}

class Succ(n: Nat) extends Nat {
  def isZero: Boolean = false
  def predecessor: Nat = n
  def successor: Nat = new Succ(this)
  def +(that: Nat): Nat =  new Succ(n + that)
  def -(that: Nat): Nat = if (that.isZero) this else n - that.predecessor
}


