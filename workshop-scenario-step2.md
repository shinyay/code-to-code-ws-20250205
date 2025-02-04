# Legacy Code Modernization Workshop

## Step 2: Analyze the COBOL Code

### Task 1: Analyze the Code Dependency

1. Open Copilot Chat with `Ctrl`+`Alt`+`I` or `Cmd`+`Alt`+`I`.

2. Ask GitHub Copilot for the following:

   Target code is `main.cobc` in the `multiple-file` directory.

    ```plaintext
    #file:main.cobc Analyze the dependencies required to execute this code.
    ```

<details>
<summary>3. GitHub Copilot Sample Response</summary>
To analyze the dependencies required to execute the COBOL code in `main.cobc-1`, follow these steps:

### 1. **Internal Dependencies**:
   - **Data Structures**: The code relies on various data structures defined in the `WORKING-STORAGE SECTION`.
   - **Subroutines**: The `PROCEDURE DIVISION` contains several subroutines that are called within the `MAIN-PROCEDURE`.

### 2. **External Dependencies**:
   - **Called Programs**: The code calls an external COBOL program `CalculateBonus` using the `CALL` statement.

### Detailed Analysis:

#### Internal Dependencies:
1. **Data Structures**:
   - `MAX-EMPLOYEES`: Defines the maximum number of employees.
   - `EMPLOYEE-TABLE`: Stores employee details.
   - `SORTED-EMPLOYEE-TABLE`: Stores sorted employee details.
   - `DEPARTMENT-TOTALS`: Stores department-wise total salaries.
   - Various other variables like `EMPLOYEE-INDEX`, `INNER-INDEX`, `TAX-RATE`, etc.

2. **Subroutines**:
   - `INITIALIZE-EMPLOYEES`: Initializes employee data.
   - `SORT-EMPLOYEES`: Sorts employees based on their IDs.
   - `CALCULATE-NET-SALARIES`: Calculates net salaries for employees.
   - `CALCULATE-DEPARTMENT-TOTALS`: Calculates total salaries for each department.
   - `DISPLAY-EMPLOYEES`: Displays employee payroll information.
   - `DISPLAY-DEPARTMENT-TOTALS`: Displays department salary totals.

#### External Dependencies:
1. **Called Programs**:
   - `CalculateBonus`: This program is called to calculate the bonus for each employee. The `CALL` statement in `CALCULATE-NET-SALARIES` subroutine indicates this dependency.

### Example of External Dependency:
```cobol
...
       CALCULATE-NET-SALARIES.
           PERFORM VARYING EMPLOYEE-INDEX FROM 1 BY 1 UNTIL EMPLOYEE-INDEX > MAX-EMPLOYEES
               MOVE SORT-GROSS-SALARY(EMPLOYEE-INDEX) TO LNK-GROSS-SALARY
               CALL 'CalculateBonus' USING LNK-GROSS-SALARY LNK-BONUS
               MOVE LNK-BONUS TO SORT-BONUS(EMPLOYEE-INDEX)
               COMPUTE SORT-DEDUCTIONS(EMPLOYEE-INDEX) = SORT-GROSS-SALARY(EMPLOYEE-INDEX) * DEDUCTION-RATE
               COMPUTE SORT-TAX-DEDUCTION(EMPLOYEE-INDEX) = SORT-GROSS-SALARY(EMPLOYEE-INDEX) * TAX-RATE
               COMPUTE SORT-NET-SALARY(EMPLOYEE-INDEX) = SORT-GROSS-SALARY(EMPLOYEE-INDEX) + SORT-BONUS(EMPLOYEE-INDEX) - SORT-TAX-DEDUCTION(EMPLOYEE-INDEX) - SORT-DEDUCTIONS(EMPLOYEE-INDEX)
           END-PERFORM.
...
```

### Summary:
To execute the code in `main.cobc-1`, ensure the following dependencies are met:
- All data structures and variables are correctly defined and initialized.
- All internal subroutines (`INITIALIZE-EMPLOYEES`, `SORT-EMPLOYEES`, etc.) are implemented.
- The external program `CalculateBonus` is available and correctly implemented.

By ensuring these dependencies are met, the `main.cobc-1` program can execute successfully.
</details>

### Task 2: Explain the code

1. Open Copilot Chat with `Ctrl`+`Alt`+`I` or `Cmd`+`Alt`+`I`.

2. Ask GitHub Copilot for the following:

   Target code is `main.cobc` in the `multiple-file` directory.

    ```plaintext
    @workspace /explain #file:main.cobc in detail.
    ```

<details>
<summary>3. GitHub Copilot Sample Response</summary>
Sure, let's go through the provided COBOL code in detail.

### Identification Division
This section identifies the program.

```cobol
IDENTIFICATION DIVISION.
       PROGRAM-ID. PayrollSystem.
```

### Data Division
This section defines the data structures and variables used in the program.

#### Working-Storage Section
This section contains variables that are used throughout the program.

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  MAX-EMPLOYEES           PIC 9(3) VALUE 5.
```

- `MAX-EMPLOYEES` is a numeric variable with a maximum value of 999, initialized to 5.

#### Employee Table
This table stores employee details.

```cobol
       01  EMPLOYEE-TABLE.
           05  EMPLOYEE-ENTRY OCCURS 5 TIMES.
               10  EMPLOYEE-ID        PIC X(5).
               10  EMPLOYEE-NAME      PIC X(20).
               10  DEPARTMENT         PIC X(10).
               10  GROSS-SALARY       PIC 9(7)V99.
               10  BONUS              PIC 9(5)V99.
               10  DEDUCTIONS         PIC 9(5)V99.
               10  NET-SALARY         PIC 9(7)V99.
               10  TAX-DEDUCTION      PIC 9(5)V99.
```

- `EMPLOYEE-ENTRY` occurs 5 times, each entry containing details like `EMPLOYEE-ID`, `EMPLOYEE-NAME`, `DEPARTMENT`, `GROSS-SALARY`, `BONUS`, `DEDUCTIONS`, `NET-SALARY`, and `TAX-DEDUCTION`.

#### Sorted Employee Table
This table stores sorted employee details.

```cobol
       01  SORTED-EMPLOYEE-TABLE.
           05  SORTED-EMPLOYEE-ENTRY OCCURS 5 TIMES.
               10  SORT-EMPLOYEE-ID   PIC X(5).
               10  SORT-EMPLOYEE-NAME PIC X(20).
               10  SORT-DEPARTMENT    PIC X(10).
               10  SORT-GROSS-SALARY  PIC 9(7)V99.
               10  SORT-BONUS         PIC 9(5)V99.
               10  SORT-DEDUCTIONS    PIC 9(5)V99.
               10  SORT-NET-SALARY    PIC 9(7)V99.
               10  SORT-TAX-DEDUCTION PIC 9(5)V99.
```

- Similar to `EMPLOYEE-TABLE`, but used for sorted data.

#### Department Totals
This table stores total salaries for each department.

```cobol
       01  DEPARTMENT-TOTALS.
           05  DEPARTMENT-TOTAL OCCURS 5 TIMES.
               10  DEPT-NAME          PIC X(10).
               10  TOTAL-SALARY       PIC 9(7)V99.
```

- `DEPARTMENT-TOTAL` occurs 5 times, each entry containing `DEPT-NAME` and `TOTAL-SALARY`.

#### Additional Variables
These variables are used for various calculations and temporary storage.

```cobol
       01  EMPLOYEE-INDEX           PIC 9(3).
       01  INNER-INDEX              PIC 9(3).
       01  TAX-RATE                 PIC 9V99 VALUE 0.20.
       01  BONUS-RATE               PIC 9V99 VALUE 0.10.
       01  DEDUCTION-RATE           PIC 9V99 VALUE 0.05.
       01  DEPARTMENT-INDEX         PIC 9(3).

       01  TEMP-ID                  PIC X(5).
       01  TEMP-NAME                PIC X(20).
       01  TEMP-DEPARTMENT          PIC X(10).
       01  TEMP-SALARY              PIC 9(7)V99.

       01  LNK-GROSS-SALARY         PIC 9(7)V99.
       01  LNK-BONUS                PIC 9(5)V99.
```

### Procedure Division
This section contains the executable code.

#### Main Procedure
The main entry point of the program.

```cobol
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-EMPLOYEES.
           PERFORM SORT-EMPLOYEES.
           PERFORM CALCULATE-NET-SALARIES.
           PERFORM CALCULATE-DEPARTMENT-TOTALS.
           PERFORM DISPLAY-EMPLOYEES.
           PERFORM DISPLAY-DEPARTMENT-TOTALS.
           STOP RUN.
```

- Calls various sub-procedures to initialize, sort, calculate, and display data.

#### Initialize Employees
Initializes the employee data.

```cobol
       INITIALIZE-EMPLOYEES.
           MOVE "E001" TO EMPLOYEE-ID(1).
           MOVE "Alice Johnson" TO EMPLOYEE-NAME(1).
           MOVE "HR" TO DEPARTMENT(1).
           MOVE 70000.00 TO GROSS-SALARY(1).
           ...
           MOVE "E005" TO EMPLOYEE-ID(5).
           MOVE "Eve Davis" TO EMPLOYEE-NAME(5).
           MOVE "HR" TO DEPARTMENT(5).
           MOVE 75000.00 TO GROSS-SALARY(5).
```

- Moves hardcoded values to the `EMPLOYEE-TABLE`.

#### Sort Employees
Sorts the employees by their IDs.

```cobol
       SORT-EMPLOYEES.
           PERFORM VARYING EMPLOYEE-INDEX FROM 1 BY 1 UNTIL EMPLOYEE-INDEX > MAX-EMPLOYEES
               MOVE EMPLOYEE-ID(EMPLOYEE-INDEX) TO SORT-EMPLOYEE-ID(EMPLOYEE-INDEX)
               ...
           END-PERFORM.
           ...
           PERFORM VARYING EMPLOYEE-INDEX FROM 1 BY 1 UNTIL EMPLOYEE-INDEX >= MAX-EMPLOYEES
               ...
               END-PERFORM.
```

- Copies data from `EMPLOYEE-TABLE` to `SORTED-EMPLOYEE-TABLE`.
- Sorts the `SORTED-EMPLOYEE-TABLE` using a bubble sort algorithm.

#### Calculate Net Salaries
Calculates the net salary for each employee.

```cobol
       CALCULATE-NET-SALARIES.
           PERFORM VARYING EMPLOYEE-INDEX FROM 1 BY 1 UNTIL EMPLOYEE-INDEX > MAX-EMPLOYEES
               MOVE SORT-GROSS-SALARY(EMPLOYEE-INDEX) TO LNK-GROSS-SALARY
               CALL 'CalculateBonus' USING LNK-GROSS-SALARY LNK-BONUS
               MOVE LNK-BONUS TO SORT-BONUS(EMPLOYEE-INDEX)
               ...
           END-PERFORM.
```

- Uses a subroutine `CalculateBonus` to compute bonuses.
- Computes deductions, tax, and net salary.

#### Calculate Department Totals
Calculates total salaries for each department.

```cobol
       CALCULATE-DEPARTMENT-TOTALS.
           PERFORM VARYING DEPARTMENT-INDEX FROM 1 BY 1 UNTIL DEPARTMENT-INDEX > 5
               MOVE SPACES TO DEPT-NAME(DEPARTMENT-INDEX)
               MOVE 0 TO TOTAL-SALARY(DEPARTMENT-INDEX)
           END-PERFORM.
           ...
           PERFORM VARYING EMPLOYEE-INDEX FROM 1 BY 1 UNTIL EMPLOYEE-INDEX > MAX-EMPLOYEES
               ...
               END-PERFORM.
```

- Initializes department totals.
- Aggregates net salaries by department.

#### Display Employees
Displays employee details.

```cobol
       DISPLAY-EMPLOYEES.
           DISPLAY "Employee Payroll Information".
           DISPLAY "-----------------------------".
           PERFORM VARYING EMPLOYEE-INDEX FROM 1 BY 1 UNTIL EMPLOYEE-INDEX > MAX-EMPLOYEES
               DISPLAY "Employee ID: " SORT-EMPLOYEE-ID(EMPLOYEE-INDEX)
               ...
           END-PERFORM.
```

- Displays sorted employee details.

#### Display Department Totals
Displays total salaries for each department.

```cobol
       DISPLAY-DEPARTMENT-TOTALS.
           DISPLAY "Department Salary Totals".
           DISPLAY "-----------------------------".
           PERFORM VARYING DEPARTMENT-INDEX FROM 1 BY 1 UNTIL DEPARTMENT-INDEX > 5
               IF DEPT-NAME(DEPARTMENT-INDEX) NOT = SPACES
                   DISPLAY "Department: " DEPT-NAME(DEPARTMENT-INDEX)
                   ...
               END-IF
           END-PERFORM.
```

- Displays department totals if the department name is not empty.

This COBOL program is a payroll system that initializes employee data, sorts it, calculates net salaries, aggregates department totals, and displays the results.
</details>

### Task 3: Identify the Subroutines

1. Open Copilot Chat with `Ctrl`+`Alt`+`I` or `Cmd`+`Alt`+`I`.

2. Ask GitHub Copilot for the following:

   Target code is `main.cobc` in the `multiple-file` directory.

    ```plaintext
    #file:main.cobc What are the subroutines defined in this code?
    ```

<details>
<summary>3. GitHub Copilot Sample Response</summary>
The subroutines defined in the main.cobc code are:

1. **MAIN-PROCEDURE**
   - This is the main entry point of the program and calls other subroutines in sequence.

2. **INITIALIZE-EMPLOYEES**
   - Initializes the employee data with hardcoded values.

3. **SORT-EMPLOYEES**
   - Sorts the employees based on their IDs using a bubble sort algorithm.

4. **CALCULATE-NET-SALARIES**
   - Calculates the net salaries for employees by calling the `CalculateBonus` program and computing deductions and tax.

5. **CALCULATE-DEPARTMENT-TOTALS**
   - Calculates total salaries for each department by aggregating net salaries.

6. **DISPLAY-EMPLOYEES**
   - Displays the sorted employee details.

7. **DISPLAY-DEPARTMENT-TOTALS**
   - Displays the total salaries for each department.

These subroutines are called in sequence from the `MAIN-PROCEDURE` to perform the various tasks required by the payroll system.
</details>

4. Ask GitHub Copilot for the following:

   Target code is `main.cobc` in the `multiple-file` directory.

    ```plaintext
    Explain each subrouines for #file:main.cobc in detail and explain the relationship of each subroutines among subroutines.
    ```
<details>
<summary>5. GitHub Copilot Sample Response</summary>
Sure, let's explain each subroutine in detail and their relationships in the main.cobc file.

### Subroutines in main.cobc

1. **MAIN-PROCEDURE**
   - **Purpose**: This is the main entry point of the program. It orchestrates the execution of other subroutines.
   - **Details**: It sequentially calls the subroutines to initialize data, sort employees, calculate net salaries, calculate department totals, and display the results.
   - **Code**:
     ```cobol
     MAIN-PROCEDURE.
         PERFORM INITIALIZE-EMPLOYEES.
         PERFORM SORT-EMPLOYEES.
         PERFORM CALCULATE-NET-SALARIES.
         PERFORM CALCULATE-DEPARTMENT-TOTALS.
         PERFORM DISPLAY-EMPLOYEES.
         PERFORM DISPLAY-DEPARTMENT-TOTALS.
         STOP RUN.
     ```

2. **INITIALIZE-EMPLOYEES**
   - **Purpose**: Initializes the employee data with hardcoded values.
   - **Details**: Moves predefined values into the `EMPLOYEE-TABLE`.
   - **Code**:
     ```cobol
     INITIALIZE-EMPLOYEES.
         MOVE "E001" TO EMPLOYEE-ID(1).
         MOVE "Alice Johnson" TO EMPLOYEE-NAME(1).
         MOVE "HR" TO DEPARTMENT(1).
         MOVE 70000.00 TO GROSS-SALARY(1).
         ...
         MOVE "E005" TO EMPLOYEE-ID(5).
         MOVE "Eve Davis" TO EMPLOYEE-NAME(5).
         MOVE "HR" TO DEPARTMENT(5).
         MOVE 75000.00 TO GROSS-SALARY(5).
     ```

3. **SORT-EMPLOYEES**
   - **Purpose**: Sorts the employees based on their IDs.
   - **Details**: Copies data from `EMPLOYEE-TABLE` to `SORTED-EMPLOYEE-TABLE` and sorts it using a bubble sort algorithm.
   - **Code**:
     ```cobol
     SORT-EMPLOYEES.
         PERFORM VARYING EMPLOYEE-INDEX FROM 1 BY 1 UNTIL EMPLOYEE-INDEX > MAX-EMPLOYEES
             MOVE EMPLOYEE-ID(EMPLOYEE-INDEX) TO SORT-EMPLOYEE-ID(EMPLOYEE-INDEX)
             MOVE EMPLOYEE-NAME(EMPLOYEE-INDEX) TO SORT-EMPLOYEE-NAME(EMPLOYEE-INDEX)
             MOVE DEPARTMENT(EMPLOYEE-INDEX) TO SORT-DEPARTMENT(EMPLOYEE-INDEX)
             MOVE GROSS-SALARY(EMPLOYEE-INDEX) TO SORT-GROSS-SALARY(EMPLOYEE-INDEX)
         END-PERFORM.

         PERFORM VARYING EMPLOYEE-INDEX FROM 1 BY 1 UNTIL EMPLOYEE-INDEX >= MAX-EMPLOYEES
             PERFORM VARYING INNER-INDEX FROM 1 BY 1 UNTIL INNER-INDEX > MAX-EMPLOYEES - EMPLOYEE-INDEX
                 IF SORT-EMPLOYEE-ID(INNER-INDEX) > SORT-EMPLOYEE-ID(INNER-INDEX + 1)
                     MOVE SORT-EMPLOYEE-ID(INNER-INDEX) TO TEMP-ID
                     MOVE SORT-EMPLOYEE-NAME(INNER-INDEX) TO TEMP-NAME
                     MOVE SORT-DEPARTMENT(INNER-INDEX) TO TEMP-DEPARTMENT
                     MOVE SORT-GROSS-SALARY(INNER-INDEX) TO TEMP-SALARY
                     MOVE SORT-EMPLOYEE-ID(INNER-INDEX + 1) TO SORT-EMPLOYEE-ID(INNER-INDEX)
                     MOVE SORT-EMPLOYEE-NAME(INNER-INDEX + 1) TO SORT-EMPLOYEE-NAME(INNER-INDEX)
                     MOVE SORT-DEPARTMENT(INNER-INDEX + 1) TO SORT-DEPARTMENT(INNER-INDEX)
                     MOVE SORT-GROSS-SALARY(INNER-INDEX + 1) TO SORT-GROSS-SALARY(INNER-INDEX)
                     MOVE TEMP-ID TO SORT-EMPLOYEE-ID(INNER-INDEX + 1)
                     MOVE TEMP-NAME TO SORT-EMPLOYEE-NAME(INNER-INDEX + 1)
                     MOVE TEMP-DEPARTMENT TO SORT-DEPARTMENT(INNER-INDEX + 1)
                     MOVE TEMP-SALARY TO SORT-GROSS-SALARY(INNER-INDEX + 1)
                 END-IF
             END-PERFORM
         END-PERFORM.
     ```

4. **CALCULATE-NET-SALARIES**
   - **Purpose**: Calculates the net salaries for employees.
   - **Details**: Calls the `CalculateBonus` program to compute bonuses and then calculates deductions, tax, and net salary.
   - **Code**:
     ```cobol
     CALCULATE-NET-SALARIES.
         PERFORM VARYING EMPLOYEE-INDEX FROM 1 BY 1 UNTIL EMPLOYEE-INDEX > MAX-EMPLOYEES
             MOVE SORT-GROSS-SALARY(EMPLOYEE-INDEX) TO LNK-GROSS-SALARY
             CALL 'CalculateBonus' USING LNK-GROSS-SALARY LNK-BONUS
             MOVE LNK-BONUS TO SORT-BONUS(EMPLOYEE-INDEX)
             COMPUTE SORT-DEDUCTIONS(EMPLOYEE-INDEX) = SORT-GROSS-SALARY(EMPLOYEE-INDEX) * DEDUCTION-RATE
             COMPUTE SORT-TAX-DEDUCTION(EMPLOYEE-INDEX) = SORT-GROSS-SALARY(EMPLOYEE-INDEX) * TAX-RATE
             COMPUTE SORT-NET-SALARY(EMPLOYEE-INDEX) = SORT-GROSS-SALARY(EMPLOYEE-INDEX) + SORT-BONUS(EMPLOYEE-INDEX) - SORT-TAX-DEDUCTION(EMPLOYEE-INDEX) - SORT-DEDUCTIONS(EMPLOYEE-INDEX)
         END-PERFORM.
     ```

5. **CALCULATE-DEPARTMENT-TOTALS**
   - **Purpose**: Calculates total salaries for each department.
   - **Details**: Initializes department totals and aggregates net salaries by department.
   - **Code**:
     ```cobol
     CALCULATE-DEPARTMENT-TOTALS.
         PERFORM VARYING DEPARTMENT-INDEX FROM 1 BY 1 UNTIL DEPARTMENT-INDEX > 5
             MOVE SPACES TO DEPT-NAME(DEPARTMENT-INDEX)
             MOVE 0 TO TOTAL-SALARY(DEPARTMENT-INDEX)
         END-PERFORM.

         PERFORM VARYING EMPLOYEE-INDEX FROM 1 BY 1 UNTIL EMPLOYEE-INDEX > MAX-EMPLOYEES
             PERFORM VARYING DEPARTMENT-INDEX FROM 1 BY 1 UNTIL DEPARTMENT-INDEX > 5
                 IF DEPT-NAME(DEPARTMENT-INDEX) = SPACES OR DEPT-NAME(DEPARTMENT-INDEX) = SORT-DEPARTMENT(EMPLOYEE-INDEX)
                     IF DEPT-NAME(DEPARTMENT-INDEX) = SPACES
                         MOVE SORT-DEPARTMENT(EMPLOYEE-INDEX) TO DEPT-NAME(DEPARTMENT-INDEX)
                     END-IF
                     ADD SORT-NET-SALARY(EMPLOYEE-INDEX) TO TOTAL-SALARY(DEPARTMENT-INDEX)
                     EXIT PERFORM
                 END-IF
             END-PERFORM
         END-PERFORM.
     ```

6. **DISPLAY-EMPLOYEES**
   - **Purpose**: Displays the sorted employee details.
   - **Details**: Iterates through the sorted employee table and displays each employee's details.
   - **Code**:
     ```cobol
     DISPLAY-EMPLOYEES.
         DISPLAY "Employee Payroll Information".
         DISPLAY "-----------------------------".
         PERFORM VARYING EMPLOYEE-INDEX FROM 1 BY 1 UNTIL EMPLOYEE-INDEX > MAX-EMPLOYEES
             DISPLAY "Employee ID: " SORT-EMPLOYEE-ID(EMPLOYEE-INDEX)
             DISPLAY "Name: " SORT-EMPLOYEE-NAME(EMPLOYEE-INDEX)
             DISPLAY "Department: " SORT-DEPARTMENT(EMPLOYEE-INDEX)
             DISPLAY "Gross Salary: $" SORT-GROSS-SALARY(EMPLOYEE-INDEX)
             DISPLAY "Bonus: $" SORT-BONUS(EMPLOYEE-INDEX)
             DISPLAY "Deductions: $" SORT-DEDUCTIONS(EMPLOYEE-INDEX)
             DISPLAY "Tax Deduction: $" SORT-TAX-DEDUCTION(EMPLOYEE-INDEX)
             DISPLAY "Net Salary: $" SORT-NET-SALARY(EMPLOYEE-INDEX)
             DISPLAY "-----------------------------"
         END-PERFORM.
     ```

7. **DISPLAY-DEPARTMENT-TOTALS**
   - **Purpose**: Displays the total salaries for each department.
   - **Details**: Iterates through the department totals and displays each department's total salary.
   - **Code**:
     ```cobol
     DISPLAY-DEPARTMENT-TOTALS.
         DISPLAY "Department Salary Totals".
         DISPLAY "-----------------------------".
         PERFORM VARYING DEPARTMENT-INDEX FROM 1 BY 1 UNTIL DEPARTMENT-INDEX > 5
             IF DEPT-NAME(DEPARTMENT-INDEX) NOT = SPACES
                 DISPLAY "Department: " DEPT-NAME(DEPARTMENT-INDEX)
                 DISPLAY "Total Salary: $" TOTAL-SALARY(DEPARTMENT-INDEX)
                 DISPLAY "-----------------------------"
             END-IF
         END-PERFORM.
     ```

### Relationship Among Subroutines

1. **MAIN-PROCEDURE**:
   - Acts as the controller, calling each subroutine in sequence to perform the overall task of the payroll system.

2. **INITIALIZE-EMPLOYEES**:
   - Sets up the initial data required for further processing.

3. **SORT-EMPLOYEES**:
   - Organizes the employee data to facilitate easier calculations and display.

4. **CALCULATE-NET-SALARIES**:
   - Uses the sorted data to compute net salaries, which are necessary for department totals and display.

5. **CALCULATE-DEPARTMENT-TOTALS**:
   - Aggregates the net salaries calculated in the previous step to provide department-wise totals.

6. **DISPLAY-EMPLOYEES**:
   - Uses the sorted and calculated data to display individual employee details.

7. **DISPLAY-DEPARTMENT-TOTALS**:
   - Uses the aggregated department totals to display overall department salary information.

Each subroutine builds upon the results of the previous ones, ensuring a structured and logical flow of data processing and display in the payroll system.
</details>
