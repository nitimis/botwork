# oduraja

Odu Raja is a single-binary, generic and open-source automation framework for acceptance testing, acceptance test driven development (ATDD), and robotic process automation (RPA). The syntax is basically plain text with parameters. Easily extendible with Rust &amp; Python. An efficient, fast alternative to Robot Framework.

# Why Odu Raja?

I have been using Robot Framework for a couple of years now. While it is a super-awesome framework, there are a couple of things that I am not very fond of:

1. It basically requires Python (and virtualenv) to run. This means, it needs more space (when building container images, for instance) and consumes a lot of memory (Python is the love of my life, but it is slow and resource-heavy).
2. The syntax could have been even more simpler. For instance, `${}`, `@{}`, etc. variable usage confuses people who are new to the framework.
3.  It is mostly extendible only with Python.

I wanted:

1. An efficient, fast, single-binary tool.
2. An even more simpler syntax than RobotFramework.
3. Extendible with Rust, Python (via PyO3), JavaScript (via neon), etc
4. Proper language defnition with PEG grammar
5. Most of all, to have fun building something that might be useful for others.  
