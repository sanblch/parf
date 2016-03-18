extern "C" {
  void start_();
  void load_trainset_(char* filename, int size);
  void load_testset_(char* filename, int size);
  void load_trees_(char* filename, int size);
  void save_trees_(char* filename, int size);
  void calculate_();
  void predict_();
  void clean_();
}
