import React from 'react';
/**
 * `componentDidCatch` and `getDerivedStateFromError`:
 * There are no Hook equivalents for these methods yet, but they will be added soon.
 * 
 * More info: https://reactjs.org/docs/hooks-faq.html#how-do-lifecycle-methods-correspond-to-hooks
 */
export default class ErrorBoundComponent extends React.PureComponent {
  constructor (props) {
    super(props);
    this.state = {
      error: null,
    };
  }

  componentDidCatch(error, errorInfo) {
    const { onError } = this.props;
    if (onError) onError(error, errorInfo);

    this.setState({ error });
  }

  render() {
    const { errorFallback, children } = this.props;
    const { error } = this.state;

    if (error) {
      return errorFallback
        || `ErrorBound component did catch unhandled exception (${error.message}) during the tree rendering`;
    }

    return children;
  }
}
