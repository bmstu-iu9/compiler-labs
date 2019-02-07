import java.util.regex.Matcher;
import java.util.regex.Pattern;

// Компиляция для Windows:
// javac -encoding utf8 IdentVsNumber.java

public class IdentVsNumber
{
	public static void main(String args[])
	{
		test_match("Альфа123");
		test_match("42");
	}

	public static void test_match(String text)
	{
		// Регулярные выражения
		String ident = "\\p{L}[\\p{L}0-9]*";
		String number = "[0-9]+";
		String pattern = "(?<ident>^"+ident+")|(?<number>^"+number+")";

		// Компиляция регулярного выражения
		Pattern p = Pattern.compile(pattern);

		// Сопоставление текста с регулярным выражением
		Matcher m = p.matcher(text);
		if (m.find()) {
			if (m.group("ident") != null) {
				System.out.println("Идентификатор " + m.group("ident"));
			} else {
				System.out.println("Число " + m.group("number"));
			}
		} else {
			System.out.println("Ошибка");
		}
	}
}
