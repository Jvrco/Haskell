-- Definindo o tipo algébrico polimórfico `Pilha`
data Pilha a = Pilha [a] deriving (Show)  -- A pilha é modelada por uma lista e pode ser exibida com deriving Show


-- Função para desempilhar um elemento (pop)
pop :: Pilha a -> Pilha a
pop (Pilha []) = error "Erro: Pilha vazia, não é possível realizar pop." -- Trata o caso de pilha vazia
pop (Pilha (_:xs)) = Pilha xs


-- Função para obter o topo da pilha (top)
top :: Pilha a -> a
top (Pilha []) = error "Erro: Pilha vazia, não é possível acessar o topo." -- Trata o caso de pilha vazia
top (Pilha (x:_)) = x

