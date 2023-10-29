package org.im97mori.ble.android.peripheral.test;

import android.os.Bundle;

import androidx.annotation.Nullable;
import androidx.fragment.app.FragmentActivity;
import androidx.fragment.app.FragmentFactory;
import androidx.fragment.app.testing.EmptyFragmentActivity;
import androidx.fragment.app.testing.FragmentFactoryHolderViewModel;

import org.im97mori.ble.android.peripheral.databinding.HiltTestActivityBinding;

import dagger.hilt.android.AndroidEntryPoint;

@AndroidEntryPoint
public class HiltTestActivity extends FragmentActivity {
    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        setTheme(getIntent().getIntExtra(EmptyFragmentActivity.THEME_EXTRAS_BUNDLE_KEY
                , androidx.fragment.testing.manifest.R.style.FragmentScenarioEmptyFragmentActivityTheme));

        FragmentFactory fragmentFactory = FragmentFactoryHolderViewModel.Companion.getInstance(this).getFragmentFactory();
        if (fragmentFactory != null) {
            getSupportFragmentManager().setFragmentFactory(fragmentFactory);
        }

        super.onCreate(savedInstanceState);

        setContentView(HiltTestActivityBinding.inflate(getLayoutInflater()).getRoot());
    }

}
