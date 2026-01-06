class A {
    int a;
    A() { }
    void set_a(int a) { this.a = a; }
}

class B extends A {
    int b;
    B() { }
    void set_a(int a) { b = a * 2; }
}

class C extends B {
    C() { }
    void set_a(int a)    { b = a * 3; }
}

class Main {
    public static void main(String args[]) {
	A a = new A();
	B b = new B();
	C c = new C();
	a.set_a(1);
	b.set_a(2);
	c.set_a(3);
	if (a.a == 1 && b.a == 0 && b.b == 4 && c.a == 0 && c.b == 9)
	    System.out.print("ok\n");
    }
}
