package hasSize

sealed trait Size {
  import Expression._

  def ==(int: Int): Expression = { checkRequirement(int); And(Not(Lt(int)), Lt(int + 1)) }
  def !=(int: Int): Expression = { checkRequirement(int); Not(this == int) }
  def <(int: Int): Expression = { checkRequirement(int); Lt(int) }
  def >(int: Int): Expression = { checkRequirement(int); Not(this <= int) }
  def <=(int: Int): Expression = { checkRequirement(int); Lt(int + 1) }
  def >=(int: Int): Expression = { checkRequirement(int); Not(this < int) }

  private def checkRequirement(int: Int): Unit =
    require(int >= 0, "Size can not be negative")
}

object Size extends Size
