package org.im97mori.ble.android.peripheral.test;

import android.os.Bundle;

import androidx.annotation.Nullable;
import androidx.fragment.app.FragmentActivity;

import org.im97mori.ble.android.peripheral.databinding.HiltTestActivityBinding;

import dagger.hilt.android.AndroidEntryPoint;

@AndroidEntryPoint
public class HiltTestActivity extends FragmentActivity {
    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        setContentView(HiltTestActivityBinding.inflate(getLayoutInflater()).getRoot());
    }

}
