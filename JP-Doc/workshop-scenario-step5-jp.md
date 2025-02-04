# レガシーコードモダナイゼーションワークショップ

## ステップ5: COBOLコードをJavaに変換する

### タスク1: クラス変換基準を定義する

1. `Ctrl`+`Alt`+`I` または `Cmd`+`Alt`+`I` を押してCopilot Chatを開きます。

2. GitHub Copilotに以下の質問をします:

   ```plaintext
   COBOLコードをJavaに再設計する際に考慮するクラス設計の基準は何ですか？
   ```

<details>
<summary>3. GitHub Copilot サンプル回答</summary>
COBOLコードをJavaに再設計する際、結果のコードが保守可能で効率的であり、Javaのオブジェクト指向機能を活用するために、いくつかの基準を考慮する必要があります。以下は主な基準です:

### 1. **カプセル化**
   - **データ隠蔽**: プライベートフィールドを使用し、データへのアクセスを制御するためにパブリックなゲッターとセッターを提供します。
   - **モジュール性**: 関連するデータと動作をクラスにグループ化して、モジュール性と再利用性を促進します。

### 2. **単一責任の原則**
   - **凝集性**: 各クラスが単一の責任を持ち、関連する機能をカプセル化するようにします。
   - **関心の分離**: 異なる関心事を異なるクラスに分離します（例: データ表現、ビジネスロジック、データアクセス）。

### 3. **継承とポリモーフィズム**
   - **階層**: 適切な場合に継承を使用してクラス階層を作成し、コードの再利用性と拡張性を促進します。
   - **インターフェースと抽象クラス**: 共通の動作を表すインターフェースや抽象クラスを定義し、ポリモーフィックな動作を可能にします。

### 4. **継承よりもコンポジション**
   - **コンポジション**: 継承よりもコンポジションを優先してコードの再利用性と柔軟性を実現します。クラス間の関係を表すためにメンバーオブジェクトを使用します。

### 5. **データ構造**
   - **コレクション**: 適切なJavaコレクション（例: `List`, `Map`, `Set`）を使用してオブジェクトのグループを管理します。
   - **データ型**: COBOLのデータ型を適切なJavaデータ型（例: `String`, `int`, `double`）にマッピングします。

### 6. **エラーハンドリング**
   - **例外**: Javaの例外処理メカニズムを使用してエラーや例外的な状況を管理します。
   - **バリデーション**: 入力のバリデーションとエラーチェックを実装してデータの整合性を確保します。

### 7. **スケーラビリティとパフォーマンス**
   - **効率性**: メモリと処理時間の観点から効率的なクラスとメソッドを設計します。
   - **スケーラビリティ**: 増加する負荷と複雑さに対応できる設計を行い、パフォーマンスの大幅な低下を防ぎます。

### 8. **保守性**
   - **可読性**: 意味のあるクラス名とメソッド名を使用して、明確で読みやすいコードを書きます。
   - **ドキュメント**: クラスとメソッドの目的と使用方法を説明するドキュメントとコメントを提供します。

### 9. **テスト可能性**
   - **ユニットテスト**: クラスとメソッドがユニットテストで簡単にテストできるように設計します。
   - **モック**: テストでモックを使用できるようにインターフェースと依存性注入を使用します。

</details>

4. `Insert into New File` をクリックします。

![image](https://github.com/user-attachments/assets/db19d43c-936e-416c-907c-ef522d40bfd3)

5. `java-conversion-criteria.md` というファイル名を入力し、保存します。

### タスク2: GitHub Copilotにクラス設計を定義させる

1. `Ctrl`+`Alt`+`I` または `Cmd`+`Alt`+`I` を押してCopilot Chatを開きます。

2. GitHub Copilotに以下の質問をします:

   ```plaintext
   設計ドキュメント #file:design-doc_main.md と変換基準 #file:java-conversion-criteria.md に基づいて、#file:main.cobc をJavaコードに変換する際に必要なクラスは何ですか？
   クラスとその決定理由を教えてください。
   ```

<details>
<summary>3. GitHub Copilot サンプル回答</summary>
設計ドキュメントと変換基準に基づいて、main.cobcのCOBOLコードをJavaに変換する際に必要なクラスを以下に示します:

### 1. **Employeeクラス**
   - **理由**: ID、名前、部門、総給与、ボーナス、控除、純給与、税控除などの属性を持つ従業員を表します。このクラスは従業員に関連するデータをカプセル化し、データのアクセスと変更のためのメソッドを提供します。

### 2. **DepartmentTotalクラス**
   - **理由**: 部門名と総給与などの属性を持つ部門の総給与を表します。このクラスは部門の給与合計に関連するデータをカプセル化し、データのアクセスと変更のためのメソッドを提供します。

### 3. **PayrollSystemクラス**
   - **理由**: 従業員の初期化、従業員のソート、純給与の計算、部門の合計の計算、情報の表示などのメソッドを含みます。このクラスは全体の給与システムを管理し、さまざまなタスクの実行を調整します。

### 4. **BonusCalculatorクラス**
   - **理由**: `CalculateBonus` COBOLプログラムの機能を表します。このクラスには総給与に基づいてボーナスを計算するメソッドが含まれています。ボーナス計算のロジックをカプセル化し、`PayrollSystem`クラスから呼び出すことができます。

### まとめ
- **Employeeクラス**: 従業員データをカプセル化し、データのアクセスと変更のためのメソッドを提供します。
- **DepartmentTotalクラス**: 部門の総給与データをカプセル化します。
- **PayrollSystemクラス**: 従業員の初期化、ソート、給与計算、情報表示など、給与システム全体を管理します。
- **BonusCalculatorクラス**: 総給与に基づいてボーナスを計算するロジックをカプセル化します。

これらのクラスは、元のCOBOLプログラムの機能を保持しながら、Javaのオブジェクト指向機能を活用することを保証します。

</details>

### タスク3: COBOLプログラムに基づくJavaクラスの詳細設計

1. `Ctrl`+`Alt`+`I` または `Cmd`+`Alt`+`I` を押してCopilot Chatを開きます。

2. GitHub Copilotに以下の質問をします:

   ```plaintext
   設計ドキュメント #file:design-doc_main.md と変換基準 #file:java-conversion-criteria.md に基づいて、Employeeクラスの詳細なクラス設計を提供してください。
   ```
<details>
<summary>3. GitHub Copilot サンプル回答</summary>

### `Employee`クラスの詳細なクラス設計

設計ドキュメントと変換基準に基づいて、Javaの`Employee`クラスの詳細なクラス設計を以下に示します:

#### 1. **クラス定義**
   - **クラス名**: `Employee`
   - **目的**: ID、名前、部門、総給与、ボーナス、控除、純給与、税控除などの属性を持つ従業員を表します。

#### 2. **属性**
   - **id**: `String`
     - 従業員のIDを表します。
   - **name**: `String`
     - 従業員の名前を表します。
   - **department**: `String`
     - 従業員が所属する部門を表します。
   - **grossSalary**: `double`
     - 従業員の総給与を表します。
   - **bonus**: `double`
     - 従業員のボーナスを表します。
   - **deductions**: `double`
     - 従業員の控除を表します。
   - **netSalary**: `double`
     - 従業員の純給与を表します。
   - **taxDeduction**: `double`
     - 従業員の税控除を表します。

#### 3. **コンストラクタ**
   - **Employee(String id, String name, String department, double grossSalary)**
     - 指定されたID、名前、部門、および総給与で従業員を初期化します。他の属性はデフォルト値に初期化されます。

#### 4. **メソッド**
   - **ゲッターとセッター**
     - `String getId()`
     - `void setId(String id)`
     - `String getName()`
     - `void setName(String name)`
     - `String getDepartment()`
     - `void setDepartment(String department)`
     - `double getGrossSalary()`
     - `void setGrossSalary(double grossSalary)`
     - `double getBonus()`
     - `void setBonus(double bonus)`
     - `double getDeductions()`
     - `void setDeductions(double deductions)`
     - `double getNetSalary()`
     - `void setNetSalary(double netSalary)`
     - `double getTaxDeduction()`
     - `void setTaxDeduction(double taxDeduction)`

#### 5. **カプセル化**
   - すべての属性はカプセル化を確保するためにプライベートです。
   - 属性にアクセスし変更するためのパブリックなゲッターとセッターが提供されます。

#### 6. **保守性**
   - クラスは明確で読みやすく、意味のある属性名を持つように設計されています。
   - クラスとそのメソッドの目的と使用方法を説明するドキュメントとコメントが提供されます。

#### 7. **テスト可能性**
   - クラスはユニットテストで簡単にテストできるように設計されています。
   - メソッドは個別にテストして正しい機能を確認できます。

### `Employee`クラスのJavaコード

```java
/**
 * ID、名前、部門、総給与、ボーナス、控除、純給与、税控除などの属性を持つ従業員を表します。
 */
public class Employee {
    // 属性
    private String id;
    private String name;
    private String department;
    private double grossSalary;
    private double bonus;
    private double deductions;
    private double netSalary;
    private double taxDeduction;

    /**
     * 指定されたID、名前、部門、および総給与で従業員を初期化します。
     * 他の属性はデフォルト値に初期化されます。
     *
     * @param id          従業員のID
     * @param name        従業員の名前
     * @param department  従業員が所属する部門
     * @param grossSalary 従業員の総給与
     */
    public Employee(String id, String name, String department, double grossSalary) {
        this.id = id;
        this.name = name;
        this.department = department;
        this.grossSalary = grossSalary;
        this.bonus = 0.0;
        this.deductions = 0.0;
        this.netSalary = 0.0;
        this.taxDeduction = 0.0;
    }

    // すべての属性のゲッターとセッター

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getDepartment() {
        return department;
    }

    public void setDepartment(String department) {
        this.department = department;
    }

    public double getGrossSalary() {
        return grossSalary;
    }

    public void setGrossSalary(double grossSalary) {
        this.grossSalary = grossSalary;
    }

    public double getBonus() {
        return bonus;
    }

    public void setBonus(double bonus) {
        this.bonus = bonus;
    }

    public double getDeductions() {
        return deductions;
    }

    public void setDeductions(double deductions) {
        this.deductions = deductions;
    }

    public double getNetSalary() {
        return netSalary;
    }

    public void setNetSalary(double netSalary) {
        this.netSalary = netSalary;
    }

    public double getTaxDeduction() {
        return taxDeduction;
    }

    public void setTaxDeduction(double taxDeduction) {
        this.taxDeduction = taxDeduction;
    }
}
```

### まとめ
- **カプセル化**: すべての属性はプライベートであり、パブリックなゲッターとセッターが提供されます。
- **単一責任の原則**: クラスは従業員データの表現にのみ焦点を当てています。
- **保守性**: クラスは明確で読みやすく、よく文書化されています。
- **テスト可能性**: クラスはユニットテストで簡単にテストできるように設計されています。

この詳細なクラス設計により、`Employee`クラスが堅牢で保守可能であり、Javaのオブジェクト指向機能を活用できることが保証されます。

</details>

4. `Insert into New File` をクリックします。

![image](https://github.com/user-attachments/assets/db19d43c-936e-416c-907c-ef522d40bfd3)

5. `employee-class.md` というファイル名を入力し、保存します。

### (オプション) タスク4: COBOLプログラムに基づくJavaクラスの詳細設計

`Employee`クラス以外の詳細なクラス設計を行います。

1. `Ctrl`+`Alt`+`I` または `Cmd`+`Alt`+`I` を押してCopilot Chatを開きます。

2. `Employee`クラスと同様にGitHub Copilotに質問します。

<details>
<summary>3. GitHub Copilot サンプル回答 - `DepartmentTotal`クラス</summary>

### `DepartmentTotal`クラスの詳細なクラス設計

設計ドキュメントと変換基準に基づいて、Javaの`DepartmentTotal`クラスの詳細なクラス設計を以下に示します:

#### 1. **クラス定義**
   - **クラス名**: `DepartmentTotal`
   - **目的**: 部門名と総給与などの属性を持つ部門の総給与を表します。

#### 2. **属性**
   - **departmentName**: `String`
     - 部門の名前を表します。
   - **totalSalary**: `double`
     - 部門の総給与を表します。

#### 3. **コンストラクタ**
   - **DepartmentTotal(String departmentName)**
     - 指定された名前で部門を初期化し、総給与を0に設定します。

#### 4. **メソッド**
   - **ゲッターとセッター**
     - `String getDepartmentName()`
     - `void setDepartmentName(String departmentName)`
     - `double getTotalSalary()`
     - `void setTotalSalary(double totalSalary)`

#### 5. **カプセル化**
   - すべての属性はカプセル化を確保するためにプライベートです。
   - 属性にアクセスし変更するためのパブリックなゲッターとセッターが提供されます。

#### 6. **保守性**
   - クラスは明確で読みやすく、意味のある属性名を持つように設計されています。
   - クラスとそのメソッドの目的と使用方法を説明するドキュメントとコメントが提供されます。

#### 7. **テスト可能性**
   - クラスはユニットテストで簡単にテストできるように設計されています。
   - メソッドは個別にテストして正しい機能を確認できます。

### `DepartmentTotal`クラスのJavaコード

```java
/**
 * 部門名と総給与などの属性を持つ部門の総給与を表します。
 */
public class DepartmentTotal {
    // 属性
    private String departmentName;
    private double totalSalary;

    /**
     * Initializes the department with the given name and sets the total salary to 0.
     *
     * @param departmentName the name of the department
     */
    public DepartmentTotal(String departmentName) {
        this.departmentName = departmentName;
        this.totalSalary = 0;
    }

    // Getters and setters for all attributes

    public String getDepartmentName() {
        return departmentName;
    }

    public void setDepartmentName(String departmentName) {
        this.departmentName = departmentName;
    }

    public double getTotalSalary() {
        return totalSalary;
    }

    public void setTotalSalary(double totalSalary) {
        this.totalSalary = totalSalary;
    }
}
```

### Summary
- **Encapsulation**: All attributes are private, and public getters and setters are provided.
- **Single Responsibility Principle**: The class focuses solely on representing department total salary data.
- **Maintainability**: The class is clear, readable, and well-documented.
- **Testability**: The class is designed to be easily testable with unit tests.

This detailed class design ensures that the `DepartmentTotal` class is robust, maintainable, and leverages the object-oriented features of Java.
</details>

<details>
<summary>4. GitHub Copilot サンプル回答 - `PayrollSystem` クラス</summary>

### `PayrollSystem` クラスの詳細なクラス設計

設計ドキュメントと変換基準に基づいて、Javaの `PayrollSystem` クラスの詳細なクラス設計を以下に示します:

#### 1. **クラス定義**
   - **クラス名**: `PayrollSystem`
   - **目的**: 従業員の初期化、従業員のソート、純給与の計算、部門の合計の計算、情報の表示など、給与システム全体を管理します。

#### 2. **属性**
   - **MAX_EMPLOYEES**: `static final int`
     - 従業員の最大数を表します。
   - **TAX_RATE**: `static final double`
     - 税率を表します。
   - **BONUS_RATE**: `static final double`
     - ボーナス率を表します。
   - **DEDUCTION_RATE**: `static final double`
     - 控除率を表します。
   - **employees**: `List<Employee>`
     - 従業員のリストを表します。
   - **departmentTotals**: `List<DepartmentTotal>`
     - 部門の合計のリストを表します。

#### 3. **メソッド**
   - **initializeEmployees()**
     - ハードコードされた値で従業員データを初期化します。
   - **sortEmployees()**
     - 従業員をIDでソートします。
   - **calculateNetSalaries()**
     - すべての従業員の純給与を計算します。
   - **calculateDepartmentTotals()**
     - 各部門の総給与を計算します。
   - **displayEmployees()**
     - 従業員の給与情報を表示します。
   - **displayDepartmentTotals()**
     - 部門の給与合計を表示します。
   - **main(String[] args)**
     - 給与システムを実行するためのメインメソッド。

#### 4. **カプセル化**
   - すべての属性はカプセル化を確保するためにプライベートです。
   - 給与システムでさまざまな操作を実行するためのパブリックなメソッドが提供されます。

#### 5. **保守性**
   - クラスは明確で読みやすく、意味のあるメソッド名を持つように設計されています。
   - クラスとそのメソッドの目的と使用方法を説明するドキュメントとコメントが提供されます。

#### 6. **テスト可能性**
   - クラスはユニットテストで簡単にテストできるように設計されています。
   - メソッドは個別にテストして正しい機能を確認できます。

### `PayrollSystem` クラスのJavaコード

```java
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

/**
 * 従業員の初期化、従業員のソート、純給与の計算、部門の合計の計算、情報の表示など、給与システム全体を管理します。
 */
public class PayrollSystem {
    // 定数
    private static final int MAX_EMPLOYEES = 5;
    private static final double TAX_RATE = 0.20;
    private static final double BONUS_RATE = 0.10;
    private static final double DEDUCTION_RATE = 0.05;

    // 属性
    private List<Employee> employees = new ArrayList<>();
    private List<DepartmentTotal> departmentTotals = new ArrayList<>();

    /**
     * ハードコードされた値で従業員データを初期化します。
     */
    public void initializeEmployees() {
        employees.add(new Employee("E001", "Alice Johnson", "HR", 70000.00));
        employees.add(new Employee("E002", "Bob Smith", "IT", 85000.00));
        employees.add(new Employee("E003", "Charlie Brown", "Finance", 60000.00));
        employees.add(new Employee("E004", "David Wilson", "IT", 95000.00));
        employees.add(new Employee("E005", "Eve Davis", "HR", 75000.00));
    }

    /**
     * 従業員をIDでソートします。
     */
    public void sortEmployees() {
        Collections.sort(employees, Comparator.comparing(Employee::getId));
    }

    /**
     * すべての従業員の純給与を計算します。
     */
    public void calculateNetSalaries() {
        BonusCalculator bonusCalculator = new BonusCalculator();
        for (Employee employee : employees) {
            double bonus = bonusCalculator.calculateBonus(employee.getGrossSalary());
            double deductions = employee.getGrossSalary() * DEDUCTION_RATE;
            double taxDeduction = employee.getGrossSalary() * TAX_RATE;
            double netSalary = employee.getGrossSalary() + bonus - taxDeduction - deductions;

            employee.setBonus(bonus);
            employee.setDeductions(deductions);
            employee.setTaxDeduction(taxDeduction);
            employee.setNetSalary(netSalary);
        }
    }

    /**
     * 各部門の総給与を計算します。
     */
    public void calculateDepartmentTotals() {
        for (Employee employee : employees) {
            DepartmentTotal departmentTotal = departmentTotals.stream()
                .filter(dt -> dt.getDepartmentName().equals(employee.getDepartment()))
                .findFirst()
                .orElseGet(() -> {
                    DepartmentTotal newDeptTotal = new DepartmentTotal(employee.getDepartment());
                    departmentTotals.add(newDeptTotal);
                    return newDeptTotal;
                });

            departmentTotal.setTotalSalary(departmentTotal.getTotalSalary() + employee.getNetSalary());
        }
    }

    /**
     * 従業員の給与情報を表示します。
     */
    public void displayEmployees() {
        System.out.println("Employee Payroll Information");
        System.out.println("-----------------------------");
        for (Employee employee : employees) {
            System.out.println("Employee ID: " + employee.getId());
            System.out.println("Name: " + employee.getName());
            System.out.println("Department: " + employee.getDepartment());
            System.out.println("Gross Salary: $" + employee.getGrossSalary());
            System.out.println("Bonus: $" + employee.getBonus());
            System.out.println("Deductions: $" + employee.getDeductions());
            System.out.println("Tax Deduction: $" + employee.getTaxDeduction());
            System.out.println("Net Salary: $" + employee.getNetSalary());
            System.out.println("-----------------------------");
        }
    }

    /**
     * 部門の給与合計を表示します。
     */
    public void displayDepartmentTotals() {
        System.out.println("Department Salary Totals");
        System.out.println("-----------------------------");
        for (DepartmentTotal departmentTotal : departmentTotals) {
            System.out.println("Department: " + departmentTotal.getDepartmentName());
            System.out.println("Total Salary: $" + departmentTotal.getTotalSalary());
            System.out.println("-----------------------------");
        }
    }

    /**
     * 給与システムを実行するためのメインメソッド。
     *
     * @param args コマンドライン引数
     */
    public static void main(String[] args) {
        PayrollSystem payrollSystem = new PayrollSystem();
        payrollSystem.initializeEmployees();
        payrollSystem.sortEmployees();
        payrollSystem.calculateNetSalaries();
        payrollSystem.calculateDepartmentTotals();
        payrollSystem.displayEmployees();
        payrollSystem.displayDepartmentTotals();
    }
}
```

### まとめ
- **カプセル化**: すべての属性はプライベートであり、パブリックなメソッドが提供されます。
- **単一責任の原則**: クラスは給与システムの管理にのみ焦点を当てています。
- **保守性**: クラスは明確で読みやすく、よく文書化されています。
- **テスト可能性**: クラスはユニットテストで簡単にテストできるように設計されています。

この詳細なクラス設計により、`PayrollSystem` クラスが堅牢で保守可能であり、Javaのオブジェクト指向機能を活用できることが保証されます。

</details>

<details>
<summary>5. GitHub Copilot サンプル回答 - BonusCalculator クラス</summary>

### `BonusCalculator` クラスの詳細なクラス設計

設計ドキュメントと変換基準に基づいて、Javaの `BonusCalculator` クラスの詳細なクラス設計を以下に示します:

#### 1. **クラス定義**
   - **クラス名**: `BonusCalculator`
   - **目的**: 総給与に基づいてボーナスを計算するロジックをカプセル化します。

#### 2. **属性**
   - **BONUS_RATE**: `static final double`
     - ボーナスを計算するために使用されるボーナス率を表します。

#### 3. **メソッド**
   - **calculateBonus(double grossSalary)**
     - 指定された総給与に基づいてボーナスを計算します。
     - **パラメータ**: `grossSalary` (double) - 従業員の総給与。
     - **戻り値**: `double` - 計算されたボーナス。

#### 4. **カプセル化**
   - ボーナス率はカプセル化を確保するためにプライベートな定数です。
   - `calculateBonus` メソッドはボーナス計算ロジックにアクセスするためにパブリックです。

#### 5. **保守性**
   - クラスは明確で読みやすく、意味のあるメソッド名を持つように設計されています。
   - クラスとそのメソッドの目的と使用方法を説明するドキュメントとコメントが提供されます。

#### 6. **テスト可能性**
   - クラスはユニットテストで簡単にテストできるように設計されています。
   - `calculateBonus` メソッドは個別にテストして正しい機能を確認できます。

### `BonusCalculator` クラスのJavaコード

```java
/**
 * 総給与に基づいてボーナスを計算するロジックをカプセル化します。
 */
public class BonusCalculator {
    // 定数
    private static final double BONUS_RATE = 0.10;

    /**
     * 指定された総給与に基づいてボーナスを計算します。
     *
     * @param grossSalary 従業員の総給与
     * @return 計算されたボーナス
     */
    public double calculateBonus(double grossSalary) {
        return grossSalary * BONUS_RATE;
    }
}
```

### まとめ
- **カプセル化**: ボーナス率はプライベートな定数であり、`calculateBonus` メソッドはパブリックです。
- **単一責任の原則**: クラスはボーナスの計算にのみ焦点を当てています。
- **保守性**: クラスは明確で読みやすく、よく文書化されています。
- **テスト可能性**: クラスはユニットテストで簡単にテストできるように設計されています。

この詳細なクラス設計により、`BonusCalculator` クラスが堅牢で保守可能であり、Javaのオブジェクト指向機能を活用できることが保証されます。

</details>
