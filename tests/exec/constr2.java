class A {
    int a1;
    int a2;
    A() { a1 = 1; a2 = 2; }
}

class Main {
    public static void main(String args[]) {
	A a = new A();
	if (a.a1 == 1 && a.a2 == 2)
	    System.out.print("ok\n");
    }
}
