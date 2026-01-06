class A {
    int n;
    boolean b;
    A a;
    String s;
    Object o;

    A() {
	if (n == 0 && ! b && a == null && s == null && o == null)
	    System.out.print("ok\n");
    }
}

class Main {
    public static void main(String args[]) {
	new A();
	int i = 0;
    }
}
