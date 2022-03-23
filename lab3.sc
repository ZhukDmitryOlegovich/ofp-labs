def sqr[T](x: T)(implicit num: Numeric[T]) = num.times(x, x);

class Circle[T](val x: T, val y: T, val r: T)(implicit num: Numeric[T]) {
	import num._

	def in(c: Circle[T]) = c.r >= r && 
		sqr(c.r - r) >= sqr(c.x - x) + sqr(c.y - y);

	// def in(c: Circle[T]) = num.gteq(c.r, r) && 
	// 	num.gteq(sqr(num.minus(c.r, r)), num.plus(sqr(num.minus(c.x, x)), sqr(num.minus(c.y, y))));

	def len()(implicit fl: Fractional[T]) = fl.toDouble(r) * 3.14 * 2;

	def area()(implicit fl: Fractional[T]) = fl.toDouble(sqr(r)) * 3.14;
}