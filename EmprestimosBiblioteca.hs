-- Simula o sistema de emprestimos de uma biblioteca utilizando Compreensão de liwstas
type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa,Livro)]


baseExemplo :: BancoDados
baseExemplo = [("Sergio","O Senhor dos Aneis"),("André","Duna"),("Fernando","Jogos Vorazes")]


livros :: BancoDados -> Pessoa -> [Livro]
livros bd p = [ll | (pp,ll) <- bd,pp==p]

emprestimos :: BancoDados -> Livros -> [Pessoa]
livros bd p = [pp | (pp,ll) <- bd,ll == l]

emprestimos :: BancoDados -> Livros -> [Pessoa]
livros bd l = (emprestimos l) /= []

qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos bd p = length (livros bd p)

emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar [] pessoa livro = [(pessoa,livro)]
emprestar ((p,l):as) pessoa livro
    | p == pessoa && l == livro =  ((p,l):as)
    |otherwise = (p,l):emprestar as pessoa livro

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver ((p,l):as) pessoa livro
    | p == pessoa && l == livro =  ([]:as)
    |otherwise = (p,l):devolver as pessoa livro
