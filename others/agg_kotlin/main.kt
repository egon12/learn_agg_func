data class P (
	val id: Long, 
	val name: String, 
	val age: Int,
	val salary: Double,
	val dept: String = "Tech",
)

fun main() {
	val p = listOf(
		P(1, "Egon", 50, 1000.0, "Finance"),
		P(2, "Gilbert", 50, 2000.0, "Finance"),
		P(3, "Nova", 37, 1500.0, "HR"),
		P(4, "Ryan", 78, 1800.0, "HR"),
		P(5, "Gone", 10, 400.0),
	)

	println("show list")
	println(p)

	println("show list of name")
	val listOfName = p.map { it.name }
	println(listOfName)

	println("sum all salary")
	val allSalary = p.map { it.salary }.sum()
	println(allSalary)

	println("name of age > 40")
	val nameAbove40 = p.filter { it.age > 40 }.map { it.name }
	println(nameAbove40)

	println("sum salary above 40")
	val sumSalaryAbove40 = p
		.filter { it.age > 40 }
		.map { it.salary }
		.sum()

	println(sumSalaryAbove40)
}



















/**
 * Using KNN to clasify the ocr label
 *
 */

val agama = listOf(
    "Islam", 
    "Budha", 
    "Kristen", 
    "Katolik", 
    "Hindu",
)

val hasilOCR = listOf(
    "Islam", 
    "Islan", 
    "Buda", 
    "Kris", 
    "Ilam", 
    "Kristen",
)

fun tebakAgama() {
	hasilOCR.map { it + ":" + tebakAgama(it) }
		.forEach { println(it) }
}

fun tebakAgama(input: String): Pair<String, Int> {
    return agama.map { Pair(it, levdist(input, it)) }
		.sortedBy { it.second }
		.first()
}

fun levdist(a: String, b: String): Int {
	return lev(a, b, a.length, b.length)
}


fun lev(a: String, b: String, m: Int, n: Int): Int {
    if (m == 0) { return n }

    if (n == 0) { return m }

    if (a[m-1] == b[n-1]) {
        return lev(a, b, m-1, n-1)
    }

    return 1 + 
        minOf(
            lev(a, b, m, n-1),
            minOf(
                lev(a, b, m-1, n),
                lev(a, b, m-1, n-1)
                )
            )
}
