import pandas as pd
import numpy as np
import arcpy

#################################
# Python Code to Append and Update Records in ArcGIS database
#################################

#Adding new data
arcpy.management.Append('new_inat', 'test_inat_db',schema_type="NO_TEST")

#Updating data
#Getting field names
def a(path):
    field_names = []
    fields = arcpy.ListFields(path)
    for field in fields:
        field_names.append(field.name)
        return field_names
  
# Setting file locations
iNat_fields = a("C://Users/nsingh/Documents/ArcGIS/Projects/Practice - iNaturalist data/updated_inat.shp")
updated_inat = "C://Users/nsingh/Documents/ArcGIS/Projects/Practice - iNaturalist data/updated_inat.shp"
test_inat_db = "C://Users/nsingh/Documents/ArcGIS/Projects/Practice - iNaturalist data/test_inat_db.shp"
iNat_dict = {}  

# Cursor to update rows
with arcpy.da.SearchCursor(updated_inat, iNat_fields) as cursor:
    for row in cursor:
        iNat_dict[row[11]] = row[1:]
with arcpy.da.UpdateCursor(test_inat_db, iNat_fields) as cursor:
    for row in cursor:
        for k, v in iNat_dict.items():
            if k == row[11]:
                row[1] = v[0]
                row[2] = v[1]
                row[3] = v[2]
                row[4] = v[3]
                row[5] = v[4]
                row[6] = v[5]
                row[7] = v[6]
                row[8] = v[7]
                row[9] = v[8]
                row[10] = v[9]
                row[11] = v[10]
                row[12] = v[11]
                row[13] = v[12]
                row[14] = v[13]
                row[15] = v[14]
                row[16] = v[15]
                row[17] = v[16]
                row[18] = v[17]
                row[19] = v[18]
                row[20] = v[19]
                row[21] = v[20]
                row[22] = v[21]
                row[23] = v[22]
                row[24] = v[23]
                row[25] = v[24]
                row[26] = v[25]
                row[27] = v[26]
                row[28] = v[27]
                row[29] = v[28]
                row[30] = v[29]
                row[31] = v[30]
                row[32] = v[31]
                row[33] = v[32]
                row[34] = v[33]
                row[35] = v[34]
                cursor.updateRow(row)


##############################
# Recreation of R work
##############################

# Importing downloaded data
df = pd.read_csv('Downloaded_Data.csv')
int_cols = list(df.select_dtypes(include="int64"))
float_cols = list(df.select_dtypes(include="float64"))
bool_cols = list(df.select_dtypes(include="bool"))
obj_cols = list(df.select_dtypes(include="object"))
df = df.astype(str)
df = df.head() #for testing only
df.loc[0,"scientific_name"] = "test" #for testing only
df.loc[1,"id"] = 1234 #for testing only
col_names = list(df.columns.values)

# Importing ArcGIS database
feature_class = "C:/Users/singh/OneDrive/Pictures/Documents/ArcGIS/Projects/iNatTest/iNatTest.gdb/inatdb"
db = arcpy.da.FeatureClassToNumPyArray(feature_class,["*"])
db = pd.DataFrame(db,columns=col_names)
pd.set_option("display.max_columns",None)
db = db.replace("None",np.NaN)
db = db.replace("NA",np.NaN)
db[float_cols] = db[float_cols].astype(float)
db[int_cols] = db[int_cols].astype(int)
db[bool_cols] = db[bool_cols].astype(str)
db[bool_cols] = db[bool_cols].replace({"TRUE":True,"FALSE":False})
db["observed_on"] = db["observed_on"].astype(str)
db[obj_cols] = db[obj_cols].astype(object)
db = db.drop(columns=["longitude","latitude"])
db = db.astype(str)
db = db.head() #for testing only

# Importing Pete's data
# Fill in later

# Finding new inat data
new_test = df["id"].isin(db["id"])
new_inat_index = new_test.index[new_test==False].tolist()
new_inat_data = df.iloc[new_inat_index]

new_inat_data[int_cols] = new_inat_data[int_cols].astype(int)
new_inat_data[float_cols] = new_inat_data[float_cols].astype(float)

# Finding updated inat data
old_inat_index = new_test.index[new_test==True].tolist()
old_inat_data = df.iloc[old_inat_index]

old_inat_comp = old_inat_data.drop(columns=["latitude","longitude","observed_on_string"])
db_comp = db.drop(columns=["observed_on_string"])

updated_test = [np.NaN]*len(old_inat_comp.index)
index = 0

for id in old_inat_comp["id"]:
    #Finding appropriate rows
    db_id_test = db_comp["id"].isin([id])
    arc_row = db_comp[db_id_test==True]
    inat_id_test = old_inat_comp["id"].isin([id])
    inat_row = old_inat_comp[inat_id_test==True]
    #Testing for differences
    equal_test = inat_row == arc_row
    equal_test = equal_test.values.tolist()[0]
    # Testing Dates
    if all(equal_test) == False:
        date_test = pd.to_datetime(arc_row["updated_at"]) <= pd.to_datetime(inat_row["updated_at"])
        if date_test.bool() == True:
            updated_test[index] = False
        else:
            updated_test[index] = True
    else:
        updated_test[index] = True
    index = index+1

false_index = [i for i, val in enumerate(updated_test) if not val]
updated_inat_data = old_inat_data.iloc[false_index]
updated_inat_data[int_cols] = updated_inat_data[int_cols].astype(int)
updated_inat_data[float_cols] = updated_inat_data[float_cols].astype(float)

print(updated_inat_data)
print(new_inat_data)

# Attempt to write dataframe to feature class - unfinished
updated_array = updated_inat_data.to_numpy()
new_array = new_inat_data.to_numpy()

arcpy.da.NumPyArrayToFeatureClass(updated_array, "C:/Users/singh/OneDrive/Pictures/Documents/ArcGIS/Projects/iNatTest/iNatTest.gdb/updated_inat", ['XY'], SR) (in_array, out_table, shape_fields, {spatial_reference})
arcpy.da.NumPyArrayToFeatureClass(new_array, "C:/Users/singh/OneDrive/Pictures/Documents/ArcGIS/Projects/iNatTest/iNatTest.gdb/new_inat", ['XY'], SR) (in_array, out_table, shape_fields, {spatial_reference})
