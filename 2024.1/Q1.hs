type ID = String
type Pessoa = String
type Equipe = [Pessoa]
type Projeto = (ID, Equipe)
type Projetos = [Projeto]

criaProjeto :: ID -> Pessoa -> Projetos -> Projetos
criaProjeto x pessoa [] = [(x, [pessoa])]  -- Caso base: Se a lista de projetos estiver vazia, cria o projeto com o membro
criaProjeto x pessoa (a:as)
    | fst a == x = (a:as)  -- Se o ID já existe, retorna a lista inalterada
    | otherwise = a : criaProjeto x pessoa as  -- Caso contrário, continua verificando o resto da lista

equipe :: ID -> Projetos -> Equipe
equipe id [] = []
equipe id (a:as)
    | fst a == id = snd a
    | otherwise equipe id as


naEquipe :: ID -> Pessoa -> Projetos -> Bool
naEquipe _ _ [] = False  -- Caso base: Se a lista de projetos estiver vazia, retorna False
naEquipe x y ((id, equipe):projetos)
    | x == id = y `elem` equipe  -- Verifica se o ID corresponde e se a pessoa está na equipe
    | otherwise = naEquipe x y projetos  -- Continua verificando nos outros projetos

acrescentarPessoa :: ID -> Pessoa -> Projetos -> Projetos
acrescentarPessoa pid pessoa [] = []
acrescentarPessoa pid pessoa ((id, equipe):projetos)
    | pid == id = (id, equipe ++ [pessoa]) : projetos
    | otherwise = (id, equipe) : acrescentarPessoa pid pessoa projetos
