# レガシーコードモダナイゼーションワークショップ

## ステップ2: COBOLコードの分析

### タスク1: コード依存関係の分析

1. `Ctrl`+`Alt`+`I` または `Cmd`+`Alt`+`I` で Copilot Chat を開きます。

2. GitHub Copilot に次の質問をします：

   ターゲットコードは `multiple-file` ディレクトリ内の `main.cobc` です。

    ```plaintext
    #file:main.cobc このコードを実行するために必要な依存関係を分析してください。
    ```

<details>
<summary>3. GitHub Copilot サンプルレスポンス</summary>
`main.cobc-1` の COBOL コードを実行するために必要な依存関係を分析するには、次の手順に従います：

### 1. **内部依存関係**:
   - **データ構造**: コードは `WORKING-STORAGE SECTION` で定義されたさまざまなデータ構造に依存しています。
   - **サブルーチン**: `PROCEDURE DIVISION` には `MAIN-PROCEDURE` 内で呼び出されるいくつかのサブルーチンが含まれています。

### 2. **外部依存関係**:
   - **呼び出されるプログラム**: コードは `CALL` ステートメントを使用して外部の COBOL プログラム `CalculateBonus` を呼び出します。

### 詳細な分析:

#### 内部依存関係:
1. **データ構造**:
   - `MAX-EMPLOYEES`: 最大従業員数を定義します。
   - `EMPLOYEE-TABLE`: 従業員の詳細を格納します。
   - `SORTED-EMPLOYEE-TABLE`: ソートされた従業員の詳細を格納します。
   - `DEPARTMENT-TOTALS`: 部門ごとの総給与を格納します。
   - その他の変数として `EMPLOYEE-INDEX`, `INNER-INDEX`, `TAX-RATE` などがあります。

2. **サブルーチン**:
   - `INITIALIZE-EMPLOYEES`: 従業員データを初期化します。
   - `SORT-EMPLOYEES`: 従業員をIDでソートします。
   - `CALCULATE-NET-SALARIES`: 従業員の純給与を計算します。
   - `CALCULATE-DEPARTMENT-TOTALS`: 各部門の総給与を計算します。
   - `DISPLAY-EMPLOYEES`: 従業員の給与情報を表示します。
   - `DISPLAY-DEPARTMENT-TOTALS`: 部門ごとの給与総額を表示します。

#### 外部依存関係:
1. **呼び出されるプログラム**:
   - `CalculateBonus`: このプログラムは各従業員のボーナスを計算するために呼び出されます。`CALCULATE-NET-SALARIES` サブルーチン内の `CALL` ステートメントがこの依存関係を示しています。

### 外部依存関係の例:
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

### まとめ:
`main.cobc-1` のコードを実行するには、次の依存関係が満たされていることを確認してください：
- すべてのデータ構造と変数が正しく定義および初期化されていること。
- すべての内部サブルーチン（`INITIALIZE-EMPLOYEES`, `SORT-EMPLOYEES` など）が実装されていること。
- 外部プログラム `CalculateBonus` が利用可能で正しく実装されていること。

これらの依存関係が満たされていることで、`main.cobc-1` プログラムは正常に実行できます。
</details>

### タスク2: コードの説明

1. `Ctrl`+`Alt`+`I` または `Cmd`+`Alt`+`I` で Copilot Chat を開きます。

2. GitHub Copilot に次の質問をします：

   ターゲットコードは `multiple-file` ディレクトリ内の `main.cobc` です。

    ```plaintext
    @workspace /explain #file:main.cobc を詳細に説明してください。
    ```

<details>
<summary>3. GitHub Copilot サンプルレスポンス</summary>
提供された COBOL コードを詳細に見ていきましょう。

### 識別部
このセクションはプログラムを識別します。

```cobol
IDENTIFICATION DIVISION.
       PROGRAM-ID. PayrollSystem.
```

### データ部
このセクションはプログラムで使用されるデータ構造と変数を定義します。

#### 作業記憶セクション
このセクションにはプログラム全体で使用される変数が含まれます。

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  MAX-EMPLOYEES           PIC 9(3) VALUE 5.
```

- `MAX-EMPLOYEES` は最大値999の数値変数で、初期値は5です。

#### 従業員テーブル
このテーブルは従業員の詳細を格納します。

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

- `EMPLOYEE-ENTRY` は5回繰り返され、それぞれのエントリには `EMPLOYEE-ID`, `EMPLOYEE-NAME`, `DEPARTMENT`, `GROSS-SALARY`, `BONUS`, `DEDUCTIONS`, `NET-SALARY`, `TAX-DEDUCTION` が含まれます。

#### ソートされた従業員テーブル
このテーブルはソートされた従業員の詳細を格納します。

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

- `EMPLOYEE-TABLE` と似ていますが、ソートされたデータ用です。

#### 部門合計
このテーブルは各部門の総給与を格納します。

```cobol
       01  DEPARTMENT-TOTALS.
           05  DEPARTMENT-TOTAL OCCURS 5 TIMES.
               10  DEPT-NAME          PIC X(10).
               10  TOTAL-SALARY       PIC 9(7)V99.
```

- `DEPARTMENT-TOTAL` は5回繰り返され、それぞれのエントリには `DEPT-NAME` と `TOTAL-SALARY` が含まれます。

#### 追加変数
これらの変数はさまざまな計算や一時的な格納に使用されます。

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

### 手続き部
このセクションには実行可能なコードが含まれます。

#### メイン手続き
プログラムのメインエントリポイントです。

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

- データの初期化、ソート、計算、表示を行うさまざまなサブルーチンを呼び出します。

#### 従業員の初期化
従業員データを初期化します。

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

- ハードコードされた値を `EMPLOYEE-TABLE` に移動します。

#### 従業員のソート
従業員をIDでソートします。

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

- `EMPLOYEE-TABLE` から `SORTED-EMPLOYEE-TABLE` にデータをコピーします。
- バブルソートアルゴリズムを使用して `SORTED-EMPLOYEE-TABLE` をソートします。

#### 純給与の計算
各従業員の純給与を計算します。

```cobol
       CALCULATE-NET-SALARIES.
           PERFORM VARYING EMPLOYEE-INDEX FROM 1 BY 1 UNTIL EMPLOYEE-INDEX > MAX-EMPLOYEES
               MOVE SORT-GROSS-SALARY(EMPLOYEE-INDEX) TO LNK-GROSS-SALARY
               CALL 'CalculateBonus' USING LNK-GROSS-SALARY LNK-BONUS
               MOVE LNK-BONUS TO SORT-BONUS(EMPLOYEE-INDEX)
               ...
           END-PERFORM.
```

- `CalculateBonus` サブルーチンを使用してボーナスを計算します。
- 控除、税金、純給与を計算します。

#### 部門合計の計算
各部門の総給与を計算します。

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

- 部門合計を初期化します。
- 部門ごとに純給与を集計します。

#### 従業員の表示
従業員の詳細を表示します。

```cobol
       DISPLAY-EMPLOYEES.
           DISPLAY "Employee Payroll Information".
           DISPLAY "-----------------------------".
           PERFORM VARYING EMPLOYEE-INDEX FROM 1 BY 1 UNTIL EMPLOYEE-INDEX > MAX-EMPLOYEES
               DISPLAY "Employee ID: " SORT-EMPLOYEE-ID(EMPLOYEE-INDEX)
               ...
           END-PERFORM.
```

- ソートされた従業員の詳細を表示します。

#### 部門合計の表示
各部門の総給与を表示します。

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

- 部門名が空でない場合に部門合計を表示します。

この COBOL プログラムは、従業員データを初期化し、ソートし、純給与を計算し、部門合計を集計し、結果を表示する給与システムです。
</details>

### タスク3: サブルーチンの特定

1. `Ctrl`+`Alt`+`I` または `Cmd`+`Alt`+`I` で Copilot Chat を開きます。

2. GitHub Copilot に次の質問をします：

   ターゲットコードは `multiple-file` ディレクトリ内の `main.cobc` です。

    ```plaintext
    #file:main.cobc #file:calculate_bonus.cobc このコードで定義されているサブルーチンは何ですか？リストアップし、それぞれについて説明をしてください。
    ```

<details>
<summary>3. GitHub Copilot サンプルレスポンス</summary>
この COBOL プログラムにはいくつかのサブルーチンが定義されています。以下にリストアップし、それぞれについて説明します。

### サブルーチンのリスト

1. **MAIN-PROCEDURE**
2. **INITIALIZE-EMPLOYEES**
3. **SORT-EMPLOYEES**
4. **CALCULATE-NET-SALARIES**
5. **CALCULATE-DEPARTMENT-TOTALS**
6. **DISPLAY-EMPLOYEES**
7. **DISPLAY-DEPARTMENT-TOTALS**
8. **CalculateBonus** (外部プログラム)

### 各サブルーチンの説明

1. **MAIN-PROCEDURE**
   - プログラムのエントリーポイントであり、他のサブルーチンを順番に呼び出して全体の処理を制御します。
   - 実行される手順は、従業員データの初期化、ソート、給与計算、部門ごとの合計計算、従業員情報の表示、部門ごとの合計表示です。

2. **INITIALIZE-EMPLOYEES**
   - 従業員データを初期化するサブルーチンです。
   - 各従業員のID、名前、部門、基本給を設定します。

3. **SORT-EMPLOYEES**
   - 従業員データをソートするサブルーチンです。
   - 従業員IDに基づいて従業員データをソートし、ソートされたデータを `SORTED-EMPLOYEE-TABLE` に格納します。

4. **CALCULATE-NET-SALARIES**
   - 各従業員のネット給与を計算するサブルーチンです。
   - 基本給に対してボーナス、控除、税金を計算し、ネット給与を算出します。
   - 外部プログラム `CalculateBonus` を呼び出してボーナスを計算します。

5. **CALCULATE-DEPARTMENT-TOTALS**
   - 部門ごとの給与合計を計算するサブルーチンです。
   - 各部門の従業員のネット給与を合計し、部門ごとの合計給与を算出します。

6. **DISPLAY-EMPLOYEES**
   - 各従業員の給与情報を表示するサブルーチンです。
   - 従業員ID、名前、部門、基本給、ボーナス、控除、税金、ネット給与を表示します。

7. **DISPLAY-DEPARTMENT-TOTALS**
   - 部門ごとの給与合計を表示するサブルーチンです。
   - 各部門の名前と合計給与を表示します。

8. **CalculateBonus** (外部プログラム)
   - 外部プログラムとして定義されており、従業員のボーナスを計算するサブルーチンです。
   - 基本給に対してボーナス率を掛けてボーナスを計算し、その結果を呼び出し元に返します。

### まとめ

これらのサブルーチンは、COBOL プログラムの各機能を分割して実装するために使用されます。各サブルーチンは特定のタスクを実行し、プログラム全体の構造を明確にし、保守性を向上させます。外部プログラム `CalculateBonus` は、他のプログラムから呼び出される独立したサブルーチンとして機能します。
</details>

4. GitHub Copilot に次の質問をします：

   ターゲットコードは `multiple-file` ディレクトリ内のコードです。

    ```plaintext
    #file:main.cobc #file:calculate_bonus.cobc 各サブルーチン間の関係を詳細に説明し、それらのシーケンスについて説明をしてください。
    ```
<details>
<summary>5. GitHub Copilot サンプルレスポンス</summary>
以下に、`main.cobc` と calculate_bonus.cobc の各サブルーチン間の関係とシーケンスについて詳細に説明します。

### main.cobc のサブルーチン

1. **MAIN-PROCEDURE**
   - プログラムのエントリーポイントであり、他のサブルーチンを順番に呼び出して全体の処理を制御します。
   - 呼び出されるサブルーチン: `INITIALIZE-EMPLOYEES`, `SORT-EMPLOYEES`, `CALCULATE-NET-SALARIES`, `CALCULATE-DEPARTMENT-TOTALS`, `DISPLAY-EMPLOYEES`, `DISPLAY-DEPARTMENT-TOTALS`

2. **INITIALIZE-EMPLOYEES**
   - 従業員データを初期化するサブルーチンです。
   - 各従業員のID、名前、部門、基本給を設定します。

3. **SORT-EMPLOYEES**
   - 従業員データをソートするサブルーチンです。
   - 従業員IDに基づいて従業員データをソートし、ソートされたデータを `SORTED-EMPLOYEE-TABLE` に格納します。

4. **CALCULATE-NET-SALARIES**
   - 各従業員のネット給与を計算するサブルーチンです。
   - 基本給に対してボーナス、控除、税金を計算し、ネット給与を算出します。
   - 外部プログラム `CalculateBonus` を呼び出してボーナスを計算します。

5. **CALCULATE-DEPARTMENT-TOTALS**
   - 部門ごとの給与合計を計算するサブルーチンです。
   - 各部門の従業員のネット給与を合計し、部門ごとの合計給与を算出します。

6. **DISPLAY-EMPLOYEES**
   - 各従業員の給与情報を表示するサブルーチンです。
   - 従業員ID、名前、部門、基本給、ボーナス、控除、税金、ネット給与を表示します。

7. **DISPLAY-DEPARTMENT-TOTALS**
   - 部門ごとの給与合計を表示するサブルーチンです。
   - 各部門の名前と合計給与を表示します。

### calculate_bonus.cobc のサブルーチン

1. **CalculateBonus**
   - 外部プログラムとして定義されており、従業員のボーナスを計算するサブルーチンです。
   - 基本給に対してボーナス率を掛けてボーナスを計算し、その結果を呼び出し元に返します。

### サブルーチン間の関係とシーケンス

1. **MAIN-PROCEDURE** が開始されると、以下の順序でサブルーチンが呼び出されます:
   - **INITIALIZE-EMPLOYEES**: 従業員データを初期化します。
   - **SORT-EMPLOYEES**: 従業員データをソートします。
   - **CALCULATE-NET-SALARIES**: 各従業員のネット給与を計算します。この中で、外部プログラム **CalculateBonus** が呼び出されます。
     - **CalculateBonus**: `CALCULATE-NET-SALARIES` 内で `CALL 'CalculateBonus' USING LNK-GROSS-SALARY LNK-BONUS` により呼び出され、ボーナスを計算します。
   - **CALCULATE-DEPARTMENT-TOTALS**: 部門ごとの給与合計を計算します。
   - **DISPLAY-EMPLOYEES**: 各従業員の給与情報を表示します。
   - **DISPLAY-DEPARTMENT-TOTALS**: 部門ごとの給与合計を表示します。

### シーケンスの詳細

1. **INITIALIZE-EMPLOYEES**
   - 従業員データを初期化します。各従業員のID、名前、部門、基本給を設定します。

2. **SORT-EMPLOYEES**
   - 従業員データをソートします。まず、`EMPLOYEE-TABLE` のデータを `SORTED-EMPLOYEE-TABLE` にコピーし、その後、バブルソートアルゴリズムを使用して従業員IDに基づいてソートします。

3. **CALCULATE-NET-SALARIES**
   - 各従業員のネット給与を計算します。まず、`SORT-GROSS-SALARY` を `LNK-GROSS-SALARY` に移動し、外部プログラム **CalculateBonus** を呼び出してボーナスを計算します。その後、控除と税金を計算し、ネット給与を算出します。

4. **CALCULATE-DEPARTMENT-TOTALS**
   - 部門ごとの給与合計を計算します。各部門の従業員のネット給与を合計し、部門ごとの合計給与を算出します。

5. **DISPLAY-EMPLOYEES**
   - 各従業員の給与情報を表示します。従業員ID、名前、部門、基本給、ボーナス、控除、税金、ネット給与を表示します。

6. **DISPLAY-DEPARTMENT-TOTALS**
   - 部門ごとの給与合計を表示します。各部門の名前と合計給与を表示します。

### まとめ

この COBOL プログラムは、従業員の給与情報を初期化、ソート、計算、表示する一連のサブルーチンで構成されています。外部プログラム **CalculateBonus** は、ボーナス計算のために呼び出され、計算結果を返します。各サブルーチンは特定のタスクを実行し、プログラム全体の処理フローを形成します。
</details>
