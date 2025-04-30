class RecordFinder
  def initialize(model_class)
    @model_class = model_class
  end
  
  def method_missing(method_name, *args)
    if method_name.to_s.start_with?('find_by_')
      attribute = method_name.to_s.sub('find_by_', '')
      @model_class.where(attribute.to_sym => args.first)
    else
      super
    end
  end
  
  def respond_to_missing?(method_name, include_private = false)
    method_name.to_s.start_with?('find_by_') || super
  end
end

# Usage
user_finder = RecordFinder.new(User)
user = user_finder.find_by_email('example@example.com')
