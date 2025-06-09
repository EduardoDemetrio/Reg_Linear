import pandas as pd
import os
from functools import reduce

# 1. Especifique o caminho para a sua pasta de trabalho
# Python no Windows também prefere barras normais (/) ou barras duplas (\\)
caminho_pasta = "C:/Users/edude/OneDrive/Área de Trabalho/Git_gustavo/Reg_Linear/Trabalho_1"

# 2. Liste todos os arquivos .csv na pasta
# O 'try-except' lida com o caso de o caminho não existir
try:
    # Cria a lista completa de caminhos para cada arquivo CSV
    arquivos_csv = [os.path.join(caminho_pasta, f) for f in os.listdir(caminho_pasta) if f.endswith('.csv')]
except FileNotFoundError:
    print(f"Erro: O diretório não foi encontrado em: {caminho_pasta}")
    arquivos_csv = []

# 3. Crie uma lista para armazenar os DataFrames processados
lista_de_dfs = []

# 4. Loop para ler e processar cada arquivo
for caminho_arquivo in arquivos_csv:
    try:
        # Leia o arquivo CSV especificando o separador e a codificação
        df = pd.read_csv(
            caminho_arquivo,
            sep='\t',
            encoding='utf-16-le' # Equivalente ao UTF-16LE do R
        )
        print(df.head(10))
        print(df.columns)
        # Verifique se a coluna 'X' existe e renomeie
        if 'Unnamed: 0' in df.columns:
            df = df.rename(columns={'Unnamed: 0': 'Municipio.Estado'})

        # Verifique se a coluna 'Municipio.Estado' existe antes de prosseguir
        if 'Municipio.Estado' not in df.columns:
            print(f"Aviso: Arquivo ignorado por não conter a coluna 'X' ou 'Municipio.Estado': {caminho_arquivo}")
            continue # Pula para o próximo arquivo no loop
        
        # Converta todas as colunas para numérico, exceto 'Municipio.Estado'
        # 'pd.to_numeric' com errors='coerce' transforma valores não numéricos em NaN (Not a Number)
        cols_para_converter = df.columns.drop('Municipio.Estado')
        df[cols_para_converter] = df[cols_para_converter].apply(pd.to_numeric, errors='coerce')
        
        # Adicione o DataFrame processado à nossa lista
        lista_de_dfs.append(df)

    except Exception as e:
        print(f"Não foi possível processar o arquivo {caminho_arquivo}. Erro: {e}")

# 5. Verifique se algum arquivo foi lido com sucesso antes de juntar
if lista_de_dfs:
    df_final = reduce(lambda left, right: pd.merge(left, right, on='Municipio.Estado', how='outer'), lista_de_dfs)

    # 6. Visualize o resultado
    print("Junção concluída com sucesso!\n")
    print(f"Dimensões do DataFrame final: {df_final.shape[0]} linhas e {df_final.shape[1]} colunas.\n")
    print("Visualização das primeiras linhas do DataFrame final:")
    print(df_final.head())
    
    print("\nEstrutura (tipos de dados) do DataFrame final:")
    df_final.info()
else:
    print("\nNenhum arquivo foi processado com sucesso. Verifique os avisos e o conteúdo dos arquivos.")
