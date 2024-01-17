run: main.jar
	java -jar main.jar


main.jar: main.kt
	kotlinc -include-runtime -d main.jar main.kt

format:
	java -jar ktfmt-0.46.jar -I . google-java-format-1.19.1.jar -F main.kt


