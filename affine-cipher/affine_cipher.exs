defmodule AffineCipher do
  # passar esse arquivo para .exs
  
  @alphabet ?a..?z

  @alphabet_length Enum.count(@alphabet)

  def encode(text, a, b) do
    find_inverse(a)

    text
    |> normalize()
    |> Enum.reduce(["", 0], fn char, acc -> 
      encode_reduce_fun(acc, char, a, b) 
    end)
    |> Enum.at(0)
  end

  defp encode_reduce_fun([acc, qtd], char, _a, _b) when char not in @alphabet do
    [acc <> <<char>>, qtd + 1]
  end

  defp encode_reduce_fun([acc, 5], _char, _a, _b) do
    [acc <> " ", 0]
  end

  defp encode_reduce_fun([acc, qtd], char, a, b) do
    result = 
      (a * find_index(char) + b)
      |> rem(@alphabet_length)
      |> at()

    [acc <> <<result>>, qtd + 1]
  end

  def decode(text, a, b) do
    inverse_mod = find_inverse(a)

    text
    |> normalize()
    |> Enum.reduce("", fn char, acc -> 
      decode_reduce_fun(acc, char, inverse_mod, b) 
    end)
  end

  defp decode_reduce_fun(acc, char, _inverse, _b) when char not in @alphabet do
    acc <> <<char>>
  end

  defp decode_reduce_fun(acc, char, inverse, b) do
    result = 
      (inverse * (find_index(char) - b))
      |> Integer.mod(@alphabet_length)
      |> at()

    acc <> <<result>>
  end

  defp find_inverse(n, count \\ 1) do
    if count > @alphabet_length do
      raise "invalid number"
    else
      if rem(n * count, @alphabet_length) == 1 do
        count
      else
        find_inverse(n, count + 1)
      end
    end
  end

  defp normalize(text) do
    text
    |> String.downcase()
    |> String.replace(" ", "")
    |> to_charlist()
  end

  defp find_index(char), do: Enum.find_index(@alphabet, &(&1 == char))

  defp at(index), do: Enum.at(@alphabet, index)
end
