import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class IdentVsNumber
{
	public static void main(String args[])
	{
		// Текст для сопоставления
		String text = "Alpha123";

		// Регулярные выражения
		String ident = "[A-Za-z][A-Za-z0-9]*";
		String number = "[0-9]+";
		String pattern = "(^"+ident+")|(^"+number+")";

		// Компиляция регулярного выражения
		Pattern p = Pattern.compile(pattern);

		// Сопоставление текста с регулярным выражением
		Matcher m = p.matcher(text);
		if (m.find()) {
			if (m.group(1) != null) {
				System.out.println("Идентификатор " + m.group(1));
			} else {
				System.out.println("Число " + m.group(2));
			}
		} else {
			System.out.println("Ошибка");
		}
	}
}
