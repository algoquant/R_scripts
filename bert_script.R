#
# Here's an example of using the Excel scripting interface
# (paste this code into the shell). 
# 
# For more information, see
#
# https://bert-toolkit.com/excel-scripting-interface-in-r
#

#
# get a reference to the workbook
#
wb <- EXCEL$Application$get_ActiveWorkbook();

#
# add a sheet
#
new.sheet <- wb$get_Sheets()$Add();

# 
# change the name of the new sheet. note that this will
# fail if a sheet with this name already exists. 
#
new.sheet$put_Name( "R Data Set" );

#
# add some data. use matrices.
#
range <- new.sheet$get_Range( "B3:F152" );
range$put_Value( as.matrix( iris ));

#
# add column headers
#
header.range <- new.sheet$get_Range( "B2:F2" );
header.range$put_Value( t( colnames( iris )));

# 
# resize columns to fit
#
range$get_EntireColumn()$AutoFit();

#
# example of using constants: add border (underline) to headers
#
borders <- header.range$get_Borders();
border <- borders$get_Item( EXCEL$XlBordersIndex$xlEdgeBottom );
border$put_Weight( EXCEL$XlBorderWeight$xlThin);

# grab the values in column B and put them in column G
datav <- new.sheet$get_Range( "B3:B152" )$get_Value()
new.sheet$get_Range( "G3:G152" )$put_Value(datav)
