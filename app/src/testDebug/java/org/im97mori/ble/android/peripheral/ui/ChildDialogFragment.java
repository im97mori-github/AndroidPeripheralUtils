package org.im97mori.ble.android.peripheral.ui;

import android.app.Dialog;
import android.os.Bundle;

import androidx.activity.ComponentDialog;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.DialogFragment;

public class ChildDialogFragment extends DialogFragment {

    public ChildDialogFragment() {
    }

    @NonNull
    @Override
    public Dialog onCreateDialog(@Nullable Bundle savedInstanceState) {
//        ArrayAdapter<String> adapter = new ArrayAdapter<>(requireActivity(), android.R.layout.simple_list_item_1, new String[]{"a", "b"});
//        return new AlertDialog.Builder(requireActivity(), getTheme())
//                .setAdapter(adapter, (dialog, which) -> {
//                })
//                .setMessage("kakfasdfa")
//                .setTitle("aiueo")
//                .create();
//        return new AlertDialog.Builder(requireActivity())
//                .setMessage("getString(R.string.order_confirmation)")
//                .setPositiveButton(getString(android.R.string.ok), (dialog, which) -> {} )
//                .create();
//        return new MaterialAlertDialogBuilder(requireActivity(), getTheme())
//                .create();
        return new ComponentDialog(requireActivity());
    }
}
