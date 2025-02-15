import Matrix._
import Dataset._
object Regression {

  def regression(dataset_file: String,
                attribute_columns: List[String],
                value_column: String,
                test_percentage: Double,
                alpha: Double,
                gradient_descent_steps: Int): (Matrix, Double) = {
    val dataset_matrix = Dataset.apply(dataset_file)
    val selected_columns = dataset_matrix.selectColumns(attribute_columns :+ value_column)
    val (train, valid) = selected_columns.split(test_percentage)

    val xTrain = Matrix(train.selectColumns(attribute_columns)) ++ 1
    val wTrain = Matrix(List(List.fill(xTrain.width.get)(0.0)).transpose)
    val yTrain = Matrix(train.selectColumn(value_column))

    def gradient_descent(steps: Int, w: Matrix): Matrix = {
      if (steps == 0) w
      else {
        val estimTrain = xTrain * w
        val err = estimTrain - yTrain
        val grad = (xTrain.transpose * err).map(_/xTrain.height.get)
        gradient_descent(steps - 1, w - grad.map(_*alpha))
      }
    }

    val W = gradient_descent(gradient_descent_steps, wTrain)
    val xValid = Matrix(valid.selectColumns(attribute_columns)) ++ 1
    val yValid = Matrix(valid.selectColumn(value_column))
    val estimValid = xValid * W
    val errValid = (estimValid - yValid).map(_.abs)
    val errFinal = errValid.data.get.head.sum / yValid.height.get
    (W, errFinal)
  }

  def main(args: Array[String]): Unit = {
    // Exemplu de utilizare
    print(regression("datasets/houseds.csv", List("GrLivArea", "YearBuilt"), "SalePrice", 0.1, 1e-7, 10000))
  }
}