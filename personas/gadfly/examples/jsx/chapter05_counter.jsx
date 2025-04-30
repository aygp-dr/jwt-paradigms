function Counter() {
  const [count, setCount] = React.useState(0);
  
  React.useEffect(() => {
    document.title = `You clicked ${count} times`;
  }, [count]);
  
  return (
    <div>
      <p>You clicked {count} times</p>
      <button onClick={() => setCount(count + 1)}>
        Click me
      </button>
    </div>
  );
}
