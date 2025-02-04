# Legacy Code Modernization Workshop

## Step 5: Convert the COBOL code to Java

### Task 1: Define the Class Conversion Criteria

1. Open Copilot Chat with `Ctrl`+`Alt`+`I` or `Cmd`+`Alt`+`I`.

2. Ask GitHub Copilot for the following:

   ```plaintext
   What criteria should be considered for class design when reworking COBOL code into Java?
   ```

<details>
<summary>3. GitHub Copilot Sample Response</summary>
When reworking COBOL code into Java, several criteria should be considered for class design to ensure the resulting code is maintainable, efficient, and leverages the object-oriented features of Java. Here are the key criteria:

### 1. **Encapsulation**
   - **Data Hiding**: Use private fields and provide public getter and setter methods to control access to the data.
   - **Modularity**: Group related data and behavior into classes to promote modularity and reusability.

### 2. **Single Responsibility Principle**
   - **Cohesion**: Ensure each class has a single responsibility and encapsulates related functionality.
   - **Separation of Concerns**: Separate different concerns into different classes (e.g., data representation, business logic, data access).

### 3. **Inheritance and Polymorphism**
   - **Hierarchy**: Use inheritance to create a class hierarchy where appropriate, promoting code reuse and extensibility.
   - **Interfaces and Abstract Classes**: Define interfaces or abstract classes to represent common behaviors and allow for polymorphic behavior.

### 4. **Composition over Inheritance**
   - **Composition**: Prefer composition over inheritance to achieve code reuse and flexibility. Use member objects to represent relationships between classes.

### 5. **Data Structures**
   - **Collections**: Use appropriate Java collections (e.g., `List`, `Map`, `Set`) to manage groups of objects.
   - **Data Types**: Map COBOL data types to appropriate Java data types (e.g., `String`, `int`, `double`).

### 6. **Error Handling**
   - **Exceptions**: Use Java's exception handling mechanism to manage errors and exceptional conditions.
   - **Validation**: Implement input validation and error checking to ensure data integrity.

### 7. **Scalability and Performance**
   - **Efficiency**: Design classes and methods to be efficient in terms of memory and processing time.
   - **Scalability**: Ensure the design can handle increased load and complexity without significant performance degradation.

### 8. **Maintainability**
   - **Readability**: Write clear and readable code with meaningful class and method names.
   - **Documentation**: Provide documentation and comments to explain the purpose and usage of classes and methods.

### 9. **Testability**
   - **Unit Testing**: Design classes and methods to be easily testable with unit tests.
   - **Mocking**: Use interfaces and dependency injection to facilitate mocking in tests.

</details>

### Task 2: Ask GitHub Copilot to define the class design

1. Open Copilot Chat with `Ctrl`+`Alt`+`I` or `Cmd`+`Alt`+`I`.

2. Ask GitHub Copilot for the following:

   ```plaintext
   Based on the design doc #file:design-doc_main.md and conversion criteria #file:java-conversion-criteria.md , what classes do we need when we convert #file:main.cobc to Java code?
   Please tell the classes and the reason why you decided.
   ```

<details>
<summary>3. GitHub Copilot Sample Response</summary>
Based on the design document and conversion criteria, we can identify the following classes needed when converting the COBOL code in main.cobc to Java:

### 1. **Employee Class**
   - **Reason**: Represents an employee with attributes like ID, name, department, gross salary, bonus, deductions, net salary, and tax deduction. This class encapsulates the data related to an employee and provides methods to access and modify the data.

### 2. **DepartmentTotal Class**
   - **Reason**: Represents the total salary for a department with attributes like department name and total salary. This class encapsulates the data related to department salary totals and provides methods to access and modify the data.

### 3. **PayrollSystem Class**
   - **Reason**: Contains methods to initialize employees, sort employees, calculate net salaries, calculate department totals, and display information. This class manages the overall payroll system and orchestrates the execution of various tasks.

### 4. **BonusCalculator Class**
   - **Reason**: Represents the functionality of the `CalculateBonus` COBOL program. This class contains a method to calculate the bonus based on the gross salary. It encapsulates the logic for bonus calculation and can be called from the `PayrollSystem` class.

### Summary
- **Employee Class**: Encapsulates employee data and provides methods to access and modify the data.
- **DepartmentTotal Class**: Encapsulates department total salary data.
- **PayrollSystem Class**: Manages the overall payroll system, including initializing employees, sorting, calculating salaries, and displaying information.
- **BonusCalculator Class**: Encapsulates the logic for calculating bonuses based on gross salary.

These classes ensure that the functionality of the original COBOL program is preserved while leveraging the object-oriented features of Java.

Similar code found with 1 license type
</details>

### Task 3: Java Class Detaild Design based on COBOL program

1. Open Copilot Chat with `Ctrl`+`Alt`+`I` or `Cmd`+`Alt`+`I`.

2. Ask GitHub Copilot for the following:

   ```plaintext
   Based on the design doc #file:design-doc_main.md and conversion criteria #file:java-conversion-criteria.md , please provide the detailed class design of Employee Class.
   ```
<details>
<summary>3. GitHub Copilot Sample Response</summary>

### Detailed Class Design for `Employee` Class

Based on the design document and conversion criteria, here is the detailed class design for the `Employee` class in Java:

#### 1. **Class Definition**
   - **Class Name**: `Employee`
   - **Purpose**: Represents an employee with attributes like ID, name, department, gross salary, bonus, deductions, net salary, and tax deduction.

#### 2. **Attributes**
   - **id**: `String`
     - Represents the employee's ID.
   - **name**: `String`
     - Represents the employee's name.
   - **department**: `String`
     - Represents the department the employee belongs to.
   - **grossSalary**: `double`
     - Represents the employee's gross salary.
   - **bonus**: `double`
     - Represents the employee's bonus.
   - **deductions**: `double`
     - Represents the employee's deductions.
   - **netSalary**: `double`
     - Represents the employee's net salary.
   - **taxDeduction**: `double`
     - Represents the employee's tax deduction.

#### 3. **Constructors**
   - **Employee(String id, String name, String department, double grossSalary)**
     - Initializes the employee with the given ID, name, department, and gross salary. Other attributes are initialized to default values.

#### 4. **Methods**
   - **Getters and Setters**
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

#### 5. **Encapsulation**
   - All attributes are private to ensure encapsulation.
   - Public getters and setters are provided to access and modify the attributes.

#### 6. **Maintainability**
   - The class is designed to be clear and readable with meaningful attribute names.
   - Documentation and comments are provided to explain the purpose and usage of the class and its methods.

#### 7. **Testability**
   - The class is designed to be easily testable with unit tests.
   - Methods can be tested individually to ensure correct functionality.

### Java Code for `Employee` Class

```java
/**
 * Represents an employee with attributes like ID, name, department, gross salary, bonus, deductions, net salary, and tax deduction.
 */
public class Employee {
    // Attributes
    private String id;
    private String name;
    private String department;
    private double grossSalary;
    private double bonus;
    private double deductions;
    private double netSalary;
    private double taxDeduction;

    /**
     * Initializes the employee with the given ID, name, department, and gross salary.
     * Other attributes are initialized to default values.
     *
     * @param id          the employee's ID
     * @param name        the employee's name
     * @param department  the department the employee belongs to
     * @param grossSalary the employee's gross salary
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

    // Getters and setters for all attributes

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

### Summary
- **Encapsulation**: All attributes are private, and public getters and setters are provided.
- **Single Responsibility Principle**: The class focuses solely on representing employee data.
- **Maintainability**: The class is clear, readable, and well-documented.
- **Testability**: The class is designed to be easily testable with unit tests.

This detailed class design ensures that the `Employee` class is robust, maintainable, and leverages the object-oriented features of Java.

</details>

### (Option) Task 4: Java Class Detaild Design based on COBOL program

Designing the detauiled class design other than `Employee` class.

1. Open Copilot Chat with `Ctrl`+`Alt`+`I` or `Cmd`+`Alt`+`I`.

2. Ask GitHub Copilot same as `Employee` class.

<details>
<summary>3. GitHub Copilot Sample Response - `DepartmentTotal` class</summary>

### Detailed Class Design for DepartmentTotal Class

Based on the design document and conversion criteria, here is the detailed class design for the `DepartmentTotal` class in Java:

#### 1. **Class Definition**
   - **Class Name**: `DepartmentTotal`
   - **Purpose**: Represents the total salary for a department with attributes like department name and total salary.

#### 2. **Attributes**
   - **departmentName**: `String`
     - Represents the name of the department.
   - **totalSalary**: `double`
     - Represents the total salary for the department.

#### 3. **Constructors**
   - **DepartmentTotal(String departmentName)**
     - Initializes the department with the given name and sets the total salary to 0.

#### 4. **Methods**
   - **Getters and Setters**
     - `String getDepartmentName()`
     - `void setDepartmentName(String departmentName)`
     - `double getTotalSalary()`
     - `void setTotalSalary(double totalSalary)`

#### 5. **Encapsulation**
   - All attributes are private to ensure encapsulation.
   - Public getters and setters are provided to access and modify the attributes.

#### 6. **Maintainability**
   - The class is designed to be clear and readable with meaningful attribute names.
   - Documentation and comments are provided to explain the purpose and usage of the class and its methods.

#### 7. **Testability**
   - The class is designed to be easily testable with unit tests.
   - Methods can be tested individually to ensure correct functionality.

### Java Code for `DepartmentTotal` Class

```java
/**
 * Represents the total salary for a department with attributes like department name and total salary.
 */
public class DepartmentTotal {
    // Attributes
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
<summary>4. GitHub Copilot Sample Response - PayrollSystem class</summary>

### Detailed Class Design for `PayrollSystem` Class

Based on the design document and conversion criteria, here is the detailed class design for the `PayrollSystem` class in Java:

#### 1. **Class Definition**
   - **Class Name**: `PayrollSystem`
   - **Purpose**: Manages the overall payroll system, including initializing employees, sorting employees, calculating net salaries, calculating department totals, and displaying information.

#### 2. **Attributes**
   - **MAX_EMPLOYEES**: `static final int`
     - Represents the maximum number of employees.
   - **TAX_RATE**: `static final double`
     - Represents the tax rate.
   - **BONUS_RATE**: `static final double`
     - Represents the bonus rate.
   - **DEDUCTION_RATE**: `static final double`
     - Represents the deduction rate.
   - **employees**: `List<Employee>`
     - Represents the list of employees.
   - **departmentTotals**: `List<DepartmentTotal>`
     - Represents the list of department totals.

#### 3. **Methods**
   - **initializeEmployees()**
     - Initializes the employee data with hardcoded values.
   - **sortEmployees()**
     - Sorts the employees by their IDs.
   - **calculateNetSalaries()**
     - Calculates the net salaries for all employees.
   - **calculateDepartmentTotals()**
     - Calculates the total salaries for each department.
   - **displayEmployees()**
     - Displays the employee payroll information.
   - **displayDepartmentTotals()**
     - Displays the department salary totals.
   - **main(String[] args)**
     - The main method to run the payroll system.

#### 4. **Encapsulation**
   - All attributes are private to ensure encapsulation.
   - Public methods are provided to perform various operations on the payroll system.

#### 5. **Maintainability**
   - The class is designed to be clear and readable with meaningful method names.
   - Documentation and comments are provided to explain the purpose and usage of the class and its methods.

#### 6. **Testability**
   - The class is designed to be easily testable with unit tests.
   - Methods can be tested individually to ensure correct functionality.

### Java Code for `PayrollSystem` Class

```java
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

/**
 * Manages the overall payroll system, including initializing employees, sorting employees, calculating net salaries,
 * calculating department totals, and displaying information.
 */
public class PayrollSystem {
    // Constants
    private static final int MAX_EMPLOYEES = 5;
    private static final double TAX_RATE = 0.20;
    private static final double BONUS_RATE = 0.10;
    private static final double DEDUCTION_RATE = 0.05;

    // Attributes
    private List<Employee> employees = new ArrayList<>();
    private List<DepartmentTotal> departmentTotals = new ArrayList<>();

    /**
     * Initializes the employee data with hardcoded values.
     */
    public void initializeEmployees() {
        employees.add(new Employee("E001", "Alice Johnson", "HR", 70000.00));
        employees.add(new Employee("E002", "Bob Smith", "IT", 85000.00));
        employees.add(new Employee("E003", "Charlie Brown", "Finance", 60000.00));
        employees.add(new Employee("E004", "David Wilson", "IT", 95000.00));
        employees.add(new Employee("E005", "Eve Davis", "HR", 75000.00));
    }

    /**
     * Sorts the employees by their IDs.
     */
    public void sortEmployees() {
        Collections.sort(employees, Comparator.comparing(Employee::getId));
    }

    /**
     * Calculates the net salaries for all employees.
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
     * Calculates the total salaries for each department.
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
     * Displays the employee payroll information.
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
     * Displays the department salary totals.
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
     * The main method to run the payroll system.
     *
     * @param args command-line arguments
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

### Summary
- **Encapsulation**: All attributes are private, and public methods are provided to perform various operations.
- **Single Responsibility Principle**: The class focuses solely on managing the payroll system.
- **Maintainability**: The class is clear, readable, and well-documented.
- **Testability**: The class is designed to be easily testable with unit tests.

This detailed class design ensures that the `PayrollSystem` class is robust, maintainable, and leverages the object-oriented features of Java.

</details>

<details>
<summary>5. GitHub Copilot Sample Response - BonusCalculator class</summary>

### Detailed Class Design for `BonusCalculator` Class

Based on the design document and conversion criteria, here is the detailed class design for the `BonusCalculator` class in Java:

#### 1. **Class Definition**
   - **Class Name**: `BonusCalculator`
   - **Purpose**: Encapsulates the logic for calculating bonuses based on gross salary.

#### 2. **Attributes**
   - **BONUS_RATE**: `static final double`
     - Represents the bonus rate used for calculating bonuses.

#### 3. **Methods**
   - **calculateBonus(double grossSalary)**
     - Calculates the bonus based on the given gross salary.
     - **Parameters**: `grossSalary` (double) - The gross salary of the employee.
     - **Returns**: `double` - The calculated bonus.

#### 4. **Encapsulation**
   - The bonus rate is a private constant to ensure encapsulation.
   - The method `calculateBonus` is public to allow access to the bonus calculation logic.

#### 5. **Maintainability**
   - The class is designed to be clear and readable with meaningful method names.
   - Documentation and comments are provided to explain the purpose and usage of the class and its methods.

#### 6. **Testability**
   - The class is designed to be easily testable with unit tests.
   - The `calculateBonus` method can be tested individually to ensure correct functionality.

### Java Code for `BonusCalculator` Class

```java
/**
 * Encapsulates the logic for calculating bonuses based on gross salary.
 */
public class BonusCalculator {
    // Constants
    private static final double BONUS_RATE = 0.10;

    /**
     * Calculates the bonus based on the given gross salary.
     *
     * @param grossSalary the gross salary of the employee
     * @return the calculated bonus
     */
    public double calculateBonus(double grossSalary) {
        return grossSalary * BONUS_RATE;
    }
}
```

### Summary
- **Encapsulation**: The bonus rate is a private constant, and the `calculateBonus` method is public.
- **Single Responsibility Principle**: The class focuses solely on calculating bonuses.
- **Maintainability**: The class is clear, readable, and well-documented.
- **Testability**: The class is designed to be easily testable with unit tests.

This detailed class design ensures that the `BonusCalculator` class is robust, maintainable, and leverages the object-oriented features of Java.
</details>
