# Projeto da Disciplina de Programação Funcional (IF708)

Alunos: Jônatas de Oliveira Clementino e Valdemiro Rosa Vieira Santos

# Chat utilizando Cloud Haskell

Uma implementação de um chat, utilizando a biblioteca do cloud haskell.


# Instalação

Foi utilizado o auxiliador de gerenciamento de projetos stack. Para instalar basta rodar "stack install".
Ele foi usado para compilar o programa e executar. Porém os executáveis já foram criados, basta utilizá-los.


# Rodando servidor

Para rodar o servidor terá que ir no diretório ".stack-work\dist\ca59d0ab\build\chatServer". Executar comando abaixo (para executar o chat na máquina local):

chatServer --host 127.0.0.1 --port 8080 --room server1

 - `host`: Endereço do servidor, nesse caso o localhost
 - `port`: porta onde o servidor vai escutar e enviar os pacotes TCP
 - `room`: nome da sala(servidor), serve mais como ID, possibilitando criar mais de um servidor. Nome da sala será passado também no cliente.


Para verificar se o servidor rodou, irá aparecer uma mensagem como esta:
```
Server launched at: 127.0.0.1:8080:0
```

# Rodando Cliente

Para rodar o cliente, a única diferença será que deve ser passado o endereço do servidor.

- `address`: Endereço do servidor, nesse caso o localhost
- `host`: Endereço do cliente, nesse caso o localhost
- `port`: porta onde o cliente vai mandar e receber as mensagens
- `room`: Nome da sala(servidor) a qual deve se conectar


para rodar basta ir no diretório ".stack-work\dist\ca59d0ab\build\chatClient" e rodar:

```
chatClient --address 127.0.0.1:8080:0 --host 127.0.0.1 --port 8880 --room server1
```

Observações: 
1) o servidor já deve estar rodando para rodar o cliente.
2) para criar vários clientes no localhost deve-se colocar número de portas diferentes de outros clientes e do servidor.
