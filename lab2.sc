// Вариант 9.
// Истинностное значение троичной логики («истина», «неизвестно», «ложь») 
// с операциями конъюнкции («*»), дизъюнкции («+») и отрицания («!»). 
// Вместо создания экземпляров класса должны быть заранее заготовлены три возможных объекта.

// equal signum
object Trilean {
	var F = new Trilean(-1)
	var U = new Trilean(0)
	var T = new Trilean(1)

	def format: Int => Trilean = {
		case 0 				=> Trilean.U
		case n if (n > 0)	=> Trilean.T
		case n 				=> Trilean.F
	}
}

class Trilean private(val v: Int) {
	def unary_! = Trilean.format(-v)

	def * (t: Trilean) = if (v > t.v) t else this

	def + (t: Trilean) = if (v < t.v) t else this
}
