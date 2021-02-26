const fmap = (f,arr) => {
    if (arr.length === 0) return arr;
    const [fst, snd] = arr;
    return [fst].concat(f(snd));
} //阳春版，无糖苦荞面
  //有待改进


const ana = f => x => {
    return fmap(ana(f),f(x))
}

module.exports = ana;


