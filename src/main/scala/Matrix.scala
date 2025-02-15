type Mat = List[List[Double]]

class Matrix(m: Option[List[List[Double]]]) {

  def transpose: Matrix = {
    m match {
      case Some(value) =>
        val transposedMatrix = value.head.indices.map(idx => value.map(row => row(idx))).toList
        Matrix(Some(transposedMatrix))
      case None => Matrix(None)
    }
  }

  def map(f: Double => Double): Matrix = {
    m match
      case None => Matrix(None)
      case Some(data) => Matrix(Some(data.map(_.map(f))))
  }
  def *(other: Matrix): Matrix = {
    (m, other.data) match
      case (Some(m1), Some(m2)) =>
        if (width != other.height) Matrix(None)
        else Matrix(m1.map(
          line => m2.transpose.map(
            col => line.zip(col).map(pair => pair._1 * pair._2).sum)))
      case _ => Matrix(None)
  }
  def ++(x: Double): Matrix = {
    m match {
      case Some(matrix) =>
        val mat = matrix.map(row => row :+ x)
        Matrix(mat)
      case _ => Matrix(None)
    }
  }
  def -(other: Matrix): Matrix = {
    (m, other.data) match {
      case (Some(m1), Some(m2)) =>
        if(height != other.height || width != other.width) Matrix(None)
        else Matrix(m1.zip(m2).map(pair => pair._1.zip(pair._2).map(pair => pair._1 - pair._2)))
      case _ => Matrix(None)
    }
  }
  def data: Option[Mat] = m
  def height: Option[Int] = m.map(_.size)
  def width: Option[Int] = {
    m match
      case None => None
      case Some(value) => Some(value.head.size)
  }
  override def toString: String = m.toString()
}

object Matrix {
  def apply(data: Mat): Matrix = new Matrix(Some(data))
  def apply(data: Option[Mat]): Matrix = new Matrix(data)
  def apply(dataset: Dataset): Matrix = {
    val matrix: Option[List[List[Double]]] = Some(dataset.data.tail.map(_.map(_.toDouble)))
    new Matrix(matrix)
  }
}
