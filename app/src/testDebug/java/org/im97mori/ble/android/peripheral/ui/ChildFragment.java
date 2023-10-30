package org.im97mori.ble.android.peripheral.ui;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import org.im97mori.ble.android.peripheral.databinding.HiltTestActivityBinding;

import javax.inject.Inject;

import dagger.hilt.android.AndroidEntryPoint;

@AndroidEntryPoint
public class ChildFragment extends Fragment {

    @Inject
    public ChildFragment() {
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        return HiltTestActivityBinding.inflate(getLayoutInflater()).getRoot();
    }
}
