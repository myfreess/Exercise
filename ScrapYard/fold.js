const foldr = (f,init,arr) => {
    if (arr.length === 0) return init;
    const [head, ...rest] = arr;
    return f(head,foldr(f,init,rest));
}

const foldl = (f,acc,arr) => {
    if (arr.length === 0) return acc;
    const [head, ...rest] = arr;
    return foldl(f,f(acc,head),rest);
}


