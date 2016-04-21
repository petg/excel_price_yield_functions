# excel_price_yield_functions

Source for Price function:
http://www.excelfunctions.net/Excel-Price-Function.html
https://support.office.com/en-us/article/PRICE-function-3ea9deac-8dfa-436f-a7c8-17ea02c21b0a

Source for Yield function:
http://www.excelfunctions.net/Excel-Yield-Function.html
https://support.office.com/en-us/article/YIELD-function-f5f5ca43-c4bd-434f-8bd2-ed3c9727a4fe


files:

1. deploy_xx.sql 
   command to run sqlplus xx_user/xx_password@tns_name @deploy_xx.sql
   deploy_xx.log will be created

2. pkg_securities.pck
     - price
     - yield

3. pkg_securities_ut.pck
   unit test package

4. test.sql
   unit tests ------- add your test cases here  ---------
