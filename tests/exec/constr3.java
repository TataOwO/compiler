class A {
    int a1;
    int a2;
    A(int a1, int b) {
	this.a1 = a1;
	a2 = b;
    }
}

class Main {
    public static void main(String args[]) {
	A a = new A(2, 3);
	if (a.a1 == 2 && a.a2 == 3)
	    System.out.print("ok\n");
    }
}
