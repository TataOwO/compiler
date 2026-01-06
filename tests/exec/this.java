class A {
    int x;

    A() { this.x = 3; }

    boolean f() { return this.x == x; }
}

class B extends A {
    int x;

    B(int x) {
	this.x = x;
	((A)this).x = x;
    }

    boolean g() { return this.f() && this.x == x; }
}

class Main {
    public static void main(String args[]) {
	B b = new B(1);
	if (new A().f() && b.f() && b.g())
	    System.out.print("ok\n");
    }
}
