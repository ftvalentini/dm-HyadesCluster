from augustus.strict import *
import pandas as pd

#%% lee data normalizada de hipparcos y transforma en dictionary
hip_sc = pd.read_csv("data/working/hip_sc.csv", index_col=0)
data_dic = hip_sc.to_dict()

#%% lee y modifica xml
with open('output/km_pmml_hipparcos_raw.xml') as f:
    lines = f.readlines()
# cambia la linea rara que corresponde
lines[2] = '<PMML version="4.1" xmlns="http://www.dmg.org/PMML-4_1">\n'
# elimina cosas que no estan implementadas en augustus
lines.remove('   <OutputField name="clusterAffinity_1" feature="clusterAffinity" value="1" dataType="double"/>\n')
lines.remove('   <OutputField name="clusterAffinity_2" feature="clusterAffinity" value="2" dataType="double"/>\n')
lines.remove('   <OutputField name="clusterAffinity_3" feature="clusterAffinity" value="3" dataType="double"/>\n')
lines.remove('   <OutputField name="clusterAffinity_4" feature="clusterAffinity" value="4" dataType="double"/>\n')

# guarda
with open('output/km_pmml_hipparcos.xml', 'w') as file:
    for l in lines:
        file.write(l)

#%% import pmml model de kmedias y correr con los mismos datos
model_km = modelLoader.loadXml("output/km_pmml_hipparcos.xml")
result = model_km.calc(data_dic)

result.output.look(columnWidth=20)
