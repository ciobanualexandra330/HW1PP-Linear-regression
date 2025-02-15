import scala.io.Source

class Dataset(m: List[List[String]]) {
  val data: List[List[String]] = m
  override def toString: String = {
    data
      .map(_.foldRight("")((x, y) => s"$x,$y"))
      .foldRight("")(_ + "\n" + _)
  }

  def selectColumn(col: String): Dataset = {
    val idx = data.head.indexOf(col)
    val newData = data.map(op => List(op(idx)))
    Dataset(newData)
  }
  def selectColumns(cols: List[String]): Dataset = {
    val idx_list = cols.map(col => data.head.indexOf(col))
    val newData = data.map(data_line => idx_list.map(idx_elem => data_line(idx_elem)))
    Dataset(newData)
  }
  def split(percentage: Double): (Dataset, Dataset) = {
    val num = (1/percentage).ceil
    val mat = data.tail
    val sortMat = mat.sortBy(_.head)
    val (train, eval) = sortMat.zipWithIndex.partition
      { case (_, index) => (index + 1) % num != 0 }
    (new Dataset(data.head :: train.map(_._1)), new Dataset(data.head :: eval.map(_._1)))
  }

  def size: Int = m.size
  def getRows: List[List[String]] = data.tail
  def getHeader: List[String] = data.head
}

object Dataset {
  def apply(csv_filename: String): Dataset = {
    val source = Source.fromFile(csv_filename)
    val data: List[List[String]] = source.getLines().map(_.split(",").toList).toList
    source.close()
    new Dataset(data)
  }

  def apply(ds: List[List[String]]): Dataset = new Dataset(ds)
}
