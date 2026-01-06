class A {
    int x;

    A() { x = 1; }

    int f(int x) {
	return x;
    }

    int g() {
	int x = 2;
	return x;
    }
}

class Main {
    public static void main(String args[]) {
	A a = new A();
	if (a.x == 1 && a.g() == 2 && a.f(3) == 3)
	    System.out.print("ok\n");
    }
}
