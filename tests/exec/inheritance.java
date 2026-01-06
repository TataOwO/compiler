
class A {
    int a;
    A() { init_a(); }
    void init_a() { a = 1; }
}

class B extends A {
    int b;
    B() { init_a(); init_b(); }
    void init_b() { b = 2; }
}

class C extends B {
    int c;
    C() { init_a(); init_b(); init_c(); }
    void init_c() { c = 3; }
}

class D extends C {
    int d;
    D() { init_a(); init_b(); init_c(); init_d(); }
    void init_d() { d = 4; }
}

class Main {
    public static void main(String args[]) {
	D d = new D();
	if (d.a == 1 && d.b == 2 && d.c == 3 && d.d == 4)
	    System.out.print("ok\n");
    }
}
