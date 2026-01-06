class A {
    String a;

    A(String s) { init(s); }

    void init(String s) { a = s; }

    String get_a() { return a; }
}

class Main {
    public static void main(String args[]) {
	A o = new A("o");
	System.out.print(o.get_a() + new A("k").get_a() + "\n");
    }
}
