class Config:
    def __init__(self):
        self.class_number = 10
        self.dropout = 0.4
        self.vector_size = 36
        self.max_length = 48
        self.batch_size = 1024*10
        self.epoch_number = 100
        self.lr = 0.001
        self.weight_decay = 5e-5
        self.max_grad_norm = 1.0