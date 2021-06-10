from math import sin, cos, sqrt, atan2
import numpy as np
import re
import pandas as pd



def calcDist(lat1, long1, lat2, long2):
    R = 6373.0
    dlon = long2 - long1
    dlat = lat2 - lat1
    a = (sin(dlat/2))**2 + cos(lat1) * cos(lat2) * (sin(dlon/2))**2
    c = 2 * atan2(sqrt(a), sqrt(1-a))
    
    return R*c

dataset = pd.read_excel(r"c:\Users\LENOVO\\Desktop\Carlos\Universidade\\SRCR\Individual\\dataset_otimo.xlsx")

dataset['PONTO_RECOLHA_LOCAL'] = dataset['PONTO_RECOLHA_LOCAL'].str.normalize('NFKD').str.encode('ascii', errors='ignore').str.decode('utf-8')
dataset['CONTENTOR_RESÍDUO'] = dataset['CONTENTOR_RESÍDUO'].str.normalize('NFKD').str.encode('ascii', errors='ignore').str.decode('utf-8')


dadosDatasetUniversal = {}
dadosDataset = {}
dadosDataset['Lixos'] = {}
dadosDataset['Vidro'] = {}
dadosDataset['Papel e Cartao'] = {}
dadosDataset['Embalagens'] = {}
dadosDataset['Organicos'] = {}


filePontos = open("pontos.pl", "w", encoding="UTF-8")
filePontos.write("%%pontos_recolha(latitude, longitude, local, tipo, quantidade, [lista]).\n\n")


for linha in dataset.values:
    
    ponto = linha[2]
    tipo = linha[3]
    res = re.search(r'([\w, -\/]+)(\[(.*)\])?', ponto)
    rua = re.split(r',', res[1])[0]
    if rua[-1] == " ":
        rua = rua[:-1]

    if rua not in dadosDataset[tipo]:
         dadosDataset[tipo][rua] = {
            'lat':linha[0],
            'long':linha[1],
            'dest':[],
            'quant':0
            }
    dadosDataset[tipo][rua]['quant'] += linha[4] 
    
    if rua not in dadosDatasetUniversal:
        dadosDatasetUniversal[rua] = {
                'lat':linha[0],
                'long':linha[1],
                'dest':[],
                'quant':0
        }
    dadosDatasetUniversal[rua]['quant'] += linha[4] 
    
    caminhos = ""
    if res[3] is not None:
        destinos = re.split(r' ?, ?', res[3])
        for destino in destinos:
            caminhos += "'" + destino + "',"
            dadosDataset[tipo][rua]['dest'].append(destino)
            if destino not in dadosDatasetUniversal[rua]['dest']:
                dadosDatasetUniversal[rua]['dest'].append(destino)
        caminhos = caminhos[:-1]
    

    filePontos.write("pontos_recolha("  + str(linha[0])
                                + ", "  + str(linha[1])
                                + ", '" + rua + "'"
                                + ", '" + tipo + "'"
                                + ", "  + str(linha[4])
                                + ", [" + str(caminhos)
                                + "]).\n")

dadosDatasetUniversal['Bqr dos Ferreiros'] = {'lat':-9.149144, 'long':38.708209, 'dest':[], 'quant':0}
dadosDatasetUniversal['Av Dom Carlos I']   = {'lat':-9.153078, 'long':38.709049, 'dest':[], 'quant':0}

fileArcos = open("arcos.pl", "w+", encoding="UTF-8")
fileArcos.write("%%arco(RUA1, RUA2, DIST).\n\n")

for (rua,valor) in dadosDatasetUniversal.items():
    for destino in valor['dest']:
        if destino in dadosDatasetUniversal:
            quantidade = calcDist(valor['lat'],valor['long'], dadosDatasetUniversal[destino]['lat'],dadosDatasetUniversal[destino]['long'])
            fileArcos.write("arco(" + "'" + rua + "'" +
                            ", " + "'" + destino + "'" +
                            ", " + str(quantidade) + ").\n")
filePontos.close()
fileArcos.close()


fileArcos_aux = open("arcos.pl", "r", encoding="UTF-8")
fileFinalArcos = open("arcos_final.pl", "w", encoding="UTF-8")
fileFinalArcos.write("%%arco(RUA1, RUA2, DIST).\n\n")

i =1
listquant =[]
for line in fileArcos_aux:

    if i>2 and i <912:
        par = re.search(r'.* ([\d.]+)\).', line)
        quant = par[1]
        if quant not in listquant:
            listquant.append(quant)
            fileFinalArcos.write(line)
    i+=1

fileArcos_aux.close()
fileFinalArcos.close()


    


                            
#fileArcos.write("%%arco(rua 1, rua 2, distancia).\n\n")




#print(dadosDataset)
# Ponto de Recolha : X
# Dicionario :
#   - Nome da Rua: resto da informação
# Arcos :
#   - Rua A para Rua B : coordenadas A e coordenadas B