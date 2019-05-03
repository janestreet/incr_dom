# An example-driven incr\_dom tutorial -- "widget" edition

This example shows how to sneak around behind the VirtualDom's back 
and create a "widget" that gives the developer full access to the DOM 
nodes and call raw mutating DOM APIs.

DO NOT USE THE WIDGET API UNLESS ABSOLUTELY NECESSARY.  

# When to use the widget API
- Never
- When interaction with third-party Javascript plugins is necessary.
- When you need to stick raw strings into the DOM as HTML.
- Never

