// Добавление элемента в начало для всех массивов
val appendBeforeAll: (Int, List[List[Int]]) => List[List[Int]] = {
	case (e, Nil)		=> Nil
	case (e, x :: xs)	=> (e :: x) :: appendBeforeAll(e, xs)
}

// Вариант 9.
// Закаренная функция framesPP: Int => (List[Int] => List[List[Int]]), формирующая список,
// состоящий из всех подсписков списка целых чисел указанной в качестве параметра функции
// длины. Подсписком будем считать список, который можно получить удалением произвольного
// количества элементов.
val framesPP: Int => List[Int] => List[List[Int]] =
n => {
	// Массива пуст => пустой массив на выход
	case Nil					=> Nil
	// Надо вернуть подмножества длины один => оборачиваем каждый элемент листа
	case x :: xs if (n == 1)	=> List(x) :: framesPP(n)(xs)
	// Надо вернуть подмножества длины равная исходному листу => это он и есть
	case l if (l.length == n)	=> List(l)
	// Надо вернуть подмножества длины больше исходного листа => нет таких
	case l if (l.length < n)	=> Nil
	// Иначе => (подмножества с первым элементом) ::: (подмножества без первого элемента)
	case x :: xs				=> appendBeforeAll(x, framesPP(n - 1)(xs)) ::: framesPP(n)(xs)
}

// =====================================================

// Обрезает лист до указанной длинны
val slice: (List[Int], Int) => List[Int] = {
	case (l, n) if (n <= 0)	=> Nil
	case (Nil, n)			=> Nil
	case (x :: xs, n)		=> x :: slice(xs, n - 1)
}

// Вариант 9.
// Закаренная функция frames: Int => (List[Int] => List[List[Int]]), формирующая список,
// состоящий из всех подсписков списка целых чисел указанной в качестве параметра функции
// длины. Подсписком будем считать список, который можно получить удалением произвольного
// количества элементов от начала и от конца списка.
val framesOld: Int => List[Int] => List[List[Int]] =
n => {
	// Массива пуст => пустой массив на выход
	case Nil							=> Nil
	// Надо вернуть подмножества длины равная исходному листу => это он и есть
	case l if (l.length == n)			=> List(l)
	// Длина некорректная => пустой массив на выход
	case l if (l.length < n || n <= 0)	=> Nil
	// Иначе => (подмножества с первым элементом) ::: (подмножества без первого элемента)
	case x :: xs						=> (x :: slice(xs, n - 1)) :: framesOld(n)(xs)
}

// =====================================================

val lenEq: (List[Int], Int) => Boolean = {
	case (Nil, n) 					=> n == 0
	case (x :: xs, n) if (n <= 0)	=> false
	case (x :: xs, n)				=> lenEq(xs, n - 1)
}

val lenLess: (List[Int], Int) => Boolean = {
	case (Nil, n) 					=> n > 0
	case (x :: xs, n) if (n <= 0)	=> false
	case (x :: xs, n)				=> lenLess(xs, n - 1)
}

val frames: Int => List[Int] => List[List[Int]] = n => {
	def carFrames: List[Int] => List[List[Int]] = if (n <= 0) l => Nil else {
		// Массива пуст => пустой массив на выход
		case Nil							=> Nil
		// Надо вернуть подмножества длины равная исходному листу => это он и есть
		case l if (lenEq(l, n))				=> List(l)
		// Длина некорректная => пустой массив на выход
		case l if (lenLess(l, n))			=> Nil
		// Иначе => (подмножества с первым элементом) ::: (подмножества без первого элемента)
		case x :: xs						=> (x :: slice(xs, n - 1)) :: carFrames(xs)
	}
	carFrames
}

// =====================================================

val testCase: (Int => List[Int] => List[List[Int]]) => (Int, List[Int]) => Unit = f => (n, l) => {
	println()
	print(n, l)
	print(" => ")
	print(f(n)(l))
	println()
}

val allTests: (Int => List[Int] => List[List[Int]]) => Unit = f => {
	val testCaseF = testCase(f)
	testCaseF(-1, List(1,2,3,4,5))
	testCaseF(0, List(1,2,3,4,5))
	testCaseF(1, List(1,2,3,4,5))
	testCaseF(2, List(1,2,3,4,5))
	testCaseF(3, List(1,2,3,4,5))
	testCaseF(4, List(1,2,3,4,5))
	testCaseF(5, List(1,2,3,4,5))
	testCaseF(6, List(1,2,3,4,5))
}

/*
$ scala
> :load ./main.scala
> allTests(frames)
> allTests(framesOld)
> allTests(framesPP)
*/
