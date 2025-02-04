# Legacy Code Modernization Workshop

## Step 3: Process of the COBOL code to a New Language

### Task 1: Ask GitHub Copilot to convert the code to Java

1. Open Copilot Chat with `Ctrl`+`Alt`+`I` or `Cmd`+`Alt`+`I`.

2. Ask GitHub Copilot for the following:

   ```plaintext
   I am looking to migrate a COBOL program to Java. I recognize that COBOL and Java have different concepts as programming languages. Therefore, I believe that I need to consider what steps I should take when converting and migrating source code. For example, I believe there are a number of considerations, such as how classes should be thought of in Java from the COBOL language, and how to migrate COBOL subroutines and copy clauses to Java. So, what are the steps to migrate from COBOL to Java?
   ```

<details>
<summary>3. GitHub Copilot Sample Response</summary>
Migrating a COBOL program to Java involves several steps to ensure that the concepts and structures in COBOL are appropriately mapped to Java. Here are the steps you should consider:

### 1. Project Planning and Preparation

- **Inventory of Codebase**:
  - Identify and document all COBOL programs, copybooks, and external dependencies.

- **Define Scope and Goals**:
  - Determine the scope of the migration and set goals (e.g., performance improvement, maintainability).

### 2. Mapping COBOL Concepts to Java

- **Program Structure**:
  - Map each COBOL program to a Java class. The `IDENTIFICATION DIVISION` in COBOL corresponds to the class declaration in Java.

- **Data Definitions**:
  - Convert data items defined in the `DATA DIVISION` to Java fields. Map COBOL data types to Java data types.

- **Subroutines**:
  - Convert COBOL subroutines (e.g., `PERFORM` and `CALL` statements) to Java methods. Treat subroutine arguments as method parameters.

- **COPYBOOK**:
  - Convert COBOL copybooks to reusable Java classes or interfaces.

### 3. Implementation of Migration

- **Create Classes**:
  - Create Java classes corresponding to each COBOL program. Name the classes based on the program names.

- **Define Fields**:
  - Define Java fields for COBOL data items. Perform data type conversions (e.g., `PIC 9(3)` to `int`).

- **Implement Methods**:
  - Implement Java methods for COBOL subroutines. Name the methods based on the subroutine names.

- **External Program Calls**:
  - Convert COBOL external program calls (`CALL` statements) to Java method calls.

### 4. Testing and Validation

- **Create Unit Tests**:
  - Develop unit tests for each Java class and method to ensure correct functionality.

- **Perform Integration Testing**:
  - Conduct integration testing to verify that the entire migrated Java codebase works correctly.

### 5. Optimization and Refactoring

- **Optimize Code**:
  - Optimize the migrated Java code for performance improvements.

- **Refactor Code**:
  - Refactor the code to improve readability and maintainability.

### Example of Converting COBOL Subroutine to Java

#### COBOL Subroutine
```cobol
INITIALIZE-EMPLOYEES.
    MOVE "E001" TO EMPLOYEE-ID(1).
    MOVE "Alice Johnson" TO EMPLOYEE-NAME(1).
    MOVE "HR" TO DEPARTMENT(1).
    MOVE 70000.00 TO GROSS-SALARY(1).
    ...
```

#### Java Method
```java
public class PayrollSystem {
    private Employee[] employees = new Employee[5];

    public void initializeEmployees() {
        employees[0] = new Employee("E001", "Alice Johnson", "HR", 70000.00);
        // Initialize other employees
    }
}

class Employee {
    private String id;
    private String name;
    private String department;
    private double grossSalary;

    public Employee(String id, String name, String department, double grossSalary) {
        this.id = id;
        this.name = name;
        this.department = department;
        this.grossSalary = grossSalary;
    }

    // Getters and setters
}
```

### Detailed Steps

1. **Inventory of Codebase**:
   - Collect all COBOL source files, copybooks, and related scripts.
   - Document the size and structure of the codebase.

2. **Define Scope and Goals**:
   - Identify which parts of the COBOL codebase will be migrated.
   - Set clear goals for the migration, such as improving performance or maintainability.

3. **Mapping COBOL Concepts to Java**:
   - **Program Structure**: Each COBOL program becomes a Java class.
   - **Data Definitions**: Convert COBOL data items to Java fields.
   - **Subroutines**: Convert COBOL subroutines to Java methods.
   - **COPYBOOK**: Convert copybooks to reusable Java classes or interfaces.

4. **Implementation of Migration**:
   - **Create Classes**: Create Java classes for each COBOL program.
   - **Define Fields**: Define Java fields for COBOL data items.
   - **Implement Methods**: Implement Java methods for COBOL subroutines.
   - **External Program Calls**: Convert external program calls to Java method calls.

5. **Testing and Validation**:
   - **Create Unit Tests**: Develop unit tests for each Java class and method.
   - **Perform Integration Testing**: Conduct integration testing to verify the entire migrated codebase.

6. **Optimization and Refactoring**:
   - **Optimize Code**: Optimize the Java code for performance.
   - **Refactor Code**: Refactor the code to improve readability and maintainability.

### Conclusion

Migrating from COBOL to Java requires careful planning and execution. By mapping COBOL concepts to Java, creating corresponding classes and methods, and thoroughly testing the migrated code, you can ensure a successful migration. Each step should be carefully documented and validated to maintain the integrity and functionality of the original COBOL program.
</details>

### Task 2: The use of GitHub Copilot in the conversion process

1. Open Copilot Chat with `Ctrl`+`Alt`+`I` or `Cmd`+`Alt`+`I`.

2. Ask GitHub Copilot for the following:

   ```plaintext
   I have reviewed the COBOL to Java migration steps. Now tell us how GitHub Copilot can help us in each of these steps.
   ```
<details>
<summary>3. GitHub Copilot Sample Response</summary>
GitHub Copilot can assist in various ways during each step of the COBOL to Java migration process. Here’s how it can help:

### 1. Project Planning and Preparation

- **Inventory of Codebase**:
  - GitHub Copilot can generate scripts to list COBOL files and calculate code metrics such as lines of code and file sizes.

  ```python
  import os

  def list_cobol_files(directory):
      cobol_files = []
      for root, _, files in os.walk(directory):
          for file in files:
              if file.endswith(('.cbl', '.cob')):
                  cobol_files.append(os.path.join(root, file))
      return cobol_files

  directory_path = '/path/to/cobol/project'
  cobol_files = list_cobol_files(directory_path)
  for file in cobol_files:
      print(file)
  ```

- **Define Scope and Goals**:
  - GitHub Copilot can help generate a migration plan document template.

  ```markdown
  # Migration Plan

  ## Scope
  - List of COBOL programs to be migrated
  - Dependencies and external systems

  ## Goals
  - Improve performance
  - Enhance maintainability
  - Reduce technical debt

  ## Timeline
  - Phase 1: Inventory and Analysis
  - Phase 2: Code Conversion
  - Phase 3: Testing and Validation
  - Phase 4: Optimization and Refactoring
  ```

### 2. Mapping COBOL Concepts to Java

- **Program Structure**:
  - GitHub Copilot can generate templates for Java classes corresponding to COBOL programs.

  ```java
  public class PayrollSystem {
      // Fields and methods will be added here
  }
  ```

- **Data Definitions**:
  - GitHub Copilot can convert COBOL data items to Java fields.

  ```java
  public class Employee {
      private String id;
      private String name;
      private String department;
      private double grossSalary;

      // Constructor, getters, and setters
  }
  ```

- **Subroutines**:
  - GitHub Copilot can convert COBOL subroutines to Java methods.

  ```java
  public void initializeEmployees() {
      employees[0] = new Employee("E001", "Alice Johnson", "HR", 70000.00);
      // Initialize other employees
  }
  ```

- **COPYBOOK**:
  - GitHub Copilot can convert COBOL copybooks to reusable Java classes or interfaces.

  ```java
  public class EmployeeData {
      // Fields and methods for reusable data structures
  }
  ```

### 3. Implementation of Migration

- **Create Classes**:
  - GitHub Copilot can generate Java classes for each COBOL program.

  ```java
  public class PayrollSystem {
      private Employee[] employees = new Employee[5];

      // Methods will be added here
  }
  ```

- **Define Fields**:
  - GitHub Copilot can convert COBOL data items to Java fields.

  ```java
  private int employeeIndex;
  private double taxRate = 0.20;
  private double bonusRate = 0.10;
  private double deductionRate = 0.05;
  ```

- **Implement Methods**:
  - GitHub Copilot can convert COBOL subroutines to Java methods.

  ```java
  public void calculateNetSalaries() {
      for (Employee employee : employees) {
          double bonus = calculateBonus(employee.getGrossSalary());
          double netSalary = employee.getGrossSalary() + bonus - (employee.getGrossSalary() * taxRate) - (employee.getGrossSalary() * deductionRate);
          employee.setNetSalary(netSalary);
      }
  }

  private double calculateBonus(double grossSalary) {
      return grossSalary * bonusRate;
  }
  ```

- **External Program Calls**:
  - GitHub Copilot can convert COBOL external program calls to Java method calls.

  ```java
  public void callCalculateBonus(double grossSalary, double[] bonus) {
      bonus[0] = calculateBonus(grossSalary);
  }
  ```

### 4. Testing and Validation

- **Create Unit Tests**:
  - GitHub Copilot can generate unit tests for each Java class and method.

  ```java
  @Test
  public void testInitializeEmployees() {
      PayrollSystem payrollSystem = new PayrollSystem();
      payrollSystem.initializeEmployees();
      assertEquals("E001", payrollSystem.getEmployees()[0].getId());
  }
  ```

- **Perform Integration Testing**:
  - GitHub Copilot can generate templates for integration tests.

  ```java
  @Test
  public void testPayrollSystemIntegration() {
      PayrollSystem payrollSystem = new PayrollSystem();
      payrollSystem.initializeEmployees();
      payrollSystem.calculateNetSalaries();
      payrollSystem.calculateDepartmentTotals();
      payrollSystem.displayEmployees();
      payrollSystem.displayDepartmentTotals();
  }
  ```

### 5. Optimization and Refactoring

- **Optimize Code**:
  - GitHub Copilot can suggest code optimizations to improve performance.

  ```java
  public void optimizeNetSalaryCalculation() {
      for (Employee employee : employees) {
          double grossSalary = employee.getGrossSalary();
          double bonus = calculateBonus(grossSalary);
          double netSalary = grossSalary + bonus - (grossSalary * taxRate) - (grossSalary * deductionRate);
          employee.setNetSalary(netSalary);
      }
  }
  ```

- **Refactor Code**:
  - GitHub Copilot can suggest refactoring to improve code readability and maintainability.

  ```java
  public class Employee {
      private String id;
      private String name;
      private String department;
      private double grossSalary;
      private double netSalary;

      // Constructor, getters, and setters
  }
  ```

### Conclusion

GitHub Copilot can assist in each step of the COBOL to Java migration process by generating code, suggesting optimizations, refactoring, and creating tests. This helps streamline the migration project and ensures a smooth transition from COBOL to Java.
</details>

### Task 3: Ask GitHub Copilot to convert the code to Python

1. Open Copilot Chat with `Ctrl`+`Alt`+`I` or `Cmd`+`Alt`+`I`.

2. Ask GitHub Copilot for the following:

   ```plaintext
   I recognize that Java, Python and COBOL are different programming models. Therefore, I believe there are different considerations related to the GAP between Python and COBOL. What steps should I take to migrate from COBOL to Python? What are the steps to migrate from COBOL to Python?
   ```
<details>
<summary>3. GitHub Copilot Sample Response</summary>
Migrating a COBOL program to Python involves several steps to ensure a smooth transition while maintaining the functionality of the original application. Here are the steps you should take:

### 1. **Understand the Existing COBOL Codebase**
   - **Inventory the Codebase**: Identify all COBOL programs, scripts, and related files.
   - **Document Functionality**: Understand and document the functionality of each COBOL program and subroutine.
   - **Identify Dependencies**: Identify internal and external dependencies, such as called programs and data sources.

### 2. **Plan the Migration**
   - **Define Scope**: Determine which parts of the COBOL codebase will be migrated.
   - **Set Objectives**: Define the goals of the migration (e.g., performance improvement, maintainability).
   - **Create a Roadmap**: Develop a detailed migration plan with timelines and milestones.

### 3. **Design the Python Architecture**
   - **Module Design**: Identify modules and functions that correspond to COBOL data structures and subroutines.
   - **Package Structure**: Organize the Python code into packages for better modularity.
   - **Error Handling**: Plan for exception handling in Python, which is different from COBOL's error handling.

### 4. **Migrate Data Structures**
   - **COBOL Records to Python Classes**: Convert COBOL record definitions to Python classes with appropriate attributes.
   - **Data Types**: Map COBOL data types to Python data types (e.g., `PIC X` to `str`, `PIC 9` to `int` or `float`).

### 5. **Migrate Subroutines**
   - **COBOL Subroutines to Python Functions**: Convert COBOL subroutines to Python functions.
   - **Function Parameters**: Use function parameters to pass data, similar to COBOL's `USING` clause.
   - **Control Structures**: Translate COBOL control structures (e.g., `PERFORM`, `IF`, `EVALUATE`) to Python control structures (`for`, `if`, `elif`).

### 6. **Implement Business Logic**
   - **Business Rules**: Ensure that all business rules and logic are correctly implemented in Python.
   - **Calculations and Algorithms**: Translate COBOL calculations and algorithms to Python.

### 7. **Handle Data Access**
   - **File I/O**: Convert COBOL file handling to Python file I/O or database access.
   - **Database Access**: Use libraries like `sqlite3`, `SQLAlchemy`, or `pandas` for database interactions.

### 8. **Testing and Validation**
   - **Unit Tests**: Write unit tests for individual Python classes and functions.
   - **Integration Tests**: Ensure that the integrated Python application works as expected.
   - **User Acceptance Testing**: Validate the migrated application with end-users to ensure it meets their requirements.

### 9. **Optimize and Refactor**
   - **Performance Tuning**: Optimize the Python code for performance.
   - **Code Refactoring**: Refactor the code to improve readability and maintainability.

### 10. **Deployment and Maintenance**
   - **Deployment Plan**: Develop a plan for deploying the Python application.
   - **Documentation**: Document the new Python codebase and provide training for the development team.
   - **Ongoing Maintenance**: Plan for ongoing maintenance and support of the Python application.

### Example: Migrating a COBOL Subroutine to Python

#### COBOL Subroutine
```cobol
INITIALIZE-EMPLOYEES.
    MOVE "E001" TO EMPLOYEE-ID(1).
    MOVE "Alice Johnson" TO EMPLOYEE-NAME(1).
    MOVE "HR" TO DEPARTMENT(1).
    MOVE 70000.00 TO GROSS-SALARY(1).
    ...
```

#### Python Function
```python
class Employee:
    def __init__(self, emp_id, name, department, gross_salary):
        self.emp_id = emp_id
        self.name = name
        self.department = department
        self.gross_salary = gross_salary

def initialize_employees():
    employees = []
    employees.append(Employee("E001", "Alice Johnson", "HR", 70000.00))
    employees.append(Employee("E002", "Bob Smith", "IT", 85000.00))
    employees.append(Employee("E003", "Charlie Brown", "Finance", 60000.00))
    employees.append(Employee("E004", "David Wilson", "Marketing", 75000.00))
    employees.append(Employee("E005", "Eve Davis", "HR", 75000.00))
    return employees

# Example usage
employees = initialize_employees()
for emp in employees:
    print(f"ID: {emp.emp_id}, Name: {emp.name}, Department: {emp.department}, Gross Salary: {emp.gross_salary}")
```

By following these steps, you can systematically migrate your COBOL program to Python, ensuring that the new application is robust, maintainable, and meets the original requirements.
</details>
