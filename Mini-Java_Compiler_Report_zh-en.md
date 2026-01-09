
# Mini-Java Compiler 專題報告

## 一、專案概述（Project Overview）

本專題實作一個 **Mini-Java compiler**。Mini-Java 為 Java 語言之子集合（subset），本編譯器以 **x86-64 assembly** 作為最終輸出目標。整體編譯流程主要分為以下兩個核心階段：

1. **Type Checking**：負責將 parser 產生之 AST（Abstract Syntax Tree）轉換為具備完整型別資訊的 typed AST。
2. **Code Generation**：將通過 type checking 的 typed AST 轉換為可執行之 x86-64 assembly code。

本專題共通過 **72 組測試案例**，在 compilation、code generation 與 runtime behavior 三個面向皆達到 **100% correct** 的結果。

---

## 二、系統設計與實作（System Design & Implementation）

### （一）Type Checking（`typing.ml`）

#### 1. Multi-pass Architecture

Type checker 採用 specification 建議之 **three-pass architecture**，以確保語意分析的完整性與正確性：

- **Pass 1 – Collect Classes**  
  蒐集所有 class names，檢查是否有 duplicate definitions，並確認 reserved names（`Object`、`String`）未被重新定義。

- **Pass 2 – Build Inheritance & Collect Members**  
  建立 `extends` inheritance 關係，使用 DFS 偵測 inheritance cycle，並收集每個 class 的 attributes 與 methods。

- **Pass 3 – Type Check Bodies**  
  在正確管理 typing environment（local variables、current class、return type）的前提下，對 constructors 與 method bodies 進行 type checking。

#### 2. Core Data Structures

- `class_table`：global hashtable，對應 class name 與其 `class_` record。
- `class_constructors`：儲存各 class constructor 的 parameter lists。
- Typing environment（`env`）：包含 local variables、current class reference 與 expected return type。

#### 3. Subtyping & Type Compatibility

- Primitive types（`int`、`boolean`）僅為自身之 subtype。
- Class types 依 inheritance chain 判定 subtype 關係。
- `null`（`typenull`）視為任何 class type 之 subtype。
- Type compatibility（τ₁ ≡ τ₂）定義為：其中一方為另一方之 subtype。

---

### （二）Code Generation（`compile.ml`）

#### 1. Memory Layout Design

**Objects**
```
[descriptor_ptr | attr1 | attr2 | ... | attrN]
```

**Class Descriptors (vtable)**
```
[parent_desc_ptr | method1_ptr | method2_ptr | ...]
```

**Strings**
```
[descriptor_ptr | null-terminated string bytes]
```

#### 2. Stack Frame Layout

- `this` 位於 `%rbp + 16`
- Method parameters 依序配置於 `%rbp + 24`, `%rbp + 32`, ...
- Local variables 配置於 `%rbp` 的 negative offsets
- Temporary values 位於 `%rsp` 所指位置

#### 3. Offset Computation Strategy

Classes 依 **topological order（parents before children）** 處理，以確保：

- Attribute offsets 能正確繼承
- Method offsets 在 vtable 中維持 prefix property，以支援 virtual method dispatch

#### 4. Virtual Method Dispatch

Method calls 透過 vtable 進行 indirect call，其流程如下：

1. Load object descriptor pointer
2. Load method pointer from descriptor at `meth.meth_ofs`
3. Invoke method via register (`call *%rcx`)

#### 5. Runtime Support Functions

以下 runtime functions 以 assembly 實作並與產生之程式碼串接：

- `_check_null`：null pointer check（method call 前）
- `_check_cast`：runtime cast check（沿 inheritance chain）
- `_instanceof`：runtime `instanceof` 判斷（回傳 0 / 1）
- `_concat_strings`：string concatenation（配置新 string）
- `_int_to_string`：integer to string conversion（透過 `sprintf`）
- Stack-aligned libc wrappers（`my_malloc`, `my_printf`, etc.）

---

## 三、實作挑戰與解決方式（Challenges & Solutions）

### 1. Parameter / Variable Offset Mismatch

**Issue：**  
Type checking 階段產生多組 `var` objects（method record 與 body environment 各一組），導致 compilation 階段設定的 parameter offsets 未套用至 method body。

**Solution：**  
修改 `typing.ml`，於 type-checking bodies 時重複使用 Pass 1 建立之 `var` objects，確保 offset 資訊一致。

---

### 2. Argument Push Order

**Issue：**  
Arguments push order 錯誤，導致 method / constructor 呼叫時 parameter values 對調。

**Solution：**  
在 push arguments 前 reverse argument list，使第一個 argument 位於最低 stack offset（closest to `this`）。

---

### 3. Class Processing Order

**Issue：**  
若 child classes 先於 parent classes 處理，會導致 method offsets 計算錯誤。

**Solution：**  
新增 `sort_by_depth`，依 inheritance depth 排序 classes，確保 parents first。

---

### 4. Void Return Exit Code

**Issue：**  
`return;` 於 `main` 中會留下 garbage value 於 `%rax`，造成 non-zero exit code。

**Solution：**  
對所有 `void return`（`Sreturn None`）明確將 `%rax` 設為 0。

---

### 5. Caller-saved Register Clobbering

**Issue：**  
呼叫 `_check_null` 後，object pointer（stored in `%rax`）遭 clobber。

**Solution：**  
於 null check 前將 object push 至 stack，call 完成後再 reload。

---

## 四、Testing Results

| Test Category | Result |
|--------------|--------|
| Syntax (Part 1) | 100% |
| Type Checking (Part 2) | 100% |
| Code Generation – Compilation | 72 / 72 |
| Code Generation – Runtime Behavior | 72 / 72 |

---

## 五、Known Limitations

- 未實作 **garbage collection (GC)**，所有 memory 皆透過 `malloc` 配置且不釋放。
- 未進行 **register allocation optimization**，所有 intermediate values 皆使用 stack。
- 不支援 arrays（不屬於 Mini-Java specification）。

---

## 六、檔案結構（File Structure）

```
mini-java-ocaml/
├── Makefile
├── ast.ml
├── lexer.mll
├── parser.mly
├── typing.ml
├── compile.ml
├── x86_64.ml / x86_64.mli
└── minijava.ml
```
