#
# Copyright (c) 2017-present, babylon health
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.
#

import numpy as np

MAXVOCAB = 100000

class FastVector:
    """
    Minimal wrapper for fastvector embeddings.
    ```
    Usage:
        $ model = FastVector(vector_file='/path/to/wiki.en.vec')
        $ 'apple' in model
        > TRUE
        $ model['apple'].shape
        > (300,)
    ```
    """

    def __init__(self, vector_file='', transform=None):
        """Read in word vectors in fasttext format"""
        self.word2id = {}
        with open(vector_file, 'r') as f:
            (self.n_words, self.n_dim) = \
                (int(x) for x in f.readline().rstrip('\n').split(' '))
            self.embed = np.zeros((MAXVOCAB, self.n_dim))
            for i, line in enumerate(f):
                if i < MAXVOCAB:
                    elems = line.rstrip('\n').split(' ')
                    self.word2id[elems[0]] = i
                    self.embed[i] = elems[1:self.n_dim+1]

        if transform is not None:
            self.apply_transform(transform)

    def apply_transform(self, transform):
        """
        Apply the given transformation to the vector space

        Right-multiplies given transform with embeddings E:
            E = E * transform

        Transform can either be a string with a filename to a
        text file containing a ndarray (compat. with np.loadtxt)
        or a numpy ndarray.
        """
        transmat = np.loadtxt(transform) if isinstance(transform, str) else transform
        self.embed = np.matmul(self.embed, transmat)

    @classmethod
    def cosine_similarity_vec(cls, vec_a, vec_b):
        """Compute cosine similarity between vec_a and vec_b"""
        return np.dot(vec_a, vec_b) / \
            (np.linalg.norm(vec_a) * np.linalg.norm(vec_b))

    def cosine_similarity(self, w1, w2):
        """Compute cosine similarity between w1 and w2"""
        v1, v2 = self.embed[self.word2id[w1]], self.embed[self.word2id[w2]]
        return self.cosine_similarity_vec(v1, v2)

    def __contains__(self, key):
        return key in self.word2id

    def __getitem__(self, key):
        return self.embed[self.word2id[key]]
