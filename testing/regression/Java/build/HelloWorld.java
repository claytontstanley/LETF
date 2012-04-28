import java.io.*;

public class HelloWorld {
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String s;
		while ((s = in.readLine()) != null && s.length() != 0) {
			System.out.println(String.format("IVs=%s", s));
			System.out.println(String.format("line=\"%s\"", s));
			System.out.println("finished=1");
		}
	}
}
