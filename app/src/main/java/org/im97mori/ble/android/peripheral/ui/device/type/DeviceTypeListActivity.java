package org.im97mori.ble.android.peripheral.ui.device.type;

import static org.im97mori.ble.android.peripheral.Constants.IntentKey.KEY_DEVICE_TYPE;

import android.content.Intent;
import android.os.Bundle;

import androidx.appcompat.app.AppCompatActivity;

import androidx.lifecycle.HasDefaultViewModelProviderFactory;
import androidx.lifecycle.ViewModelProvider;
import org.im97mori.ble.android.peripheral.databinding.DeviceTypeListActivityBinding;
import org.im97mori.ble.android.peripheral.utils.AutoDisposeViewModelProvider;

import java.util.Objects;
import java.util.function.Function;

import javax.annotation.Nullable;
import javax.inject.Inject;

import dagger.hilt.android.AndroidEntryPoint;

@AndroidEntryPoint
public class DeviceTypeListActivity extends AppCompatActivity {

    @Inject
    Function<HasDefaultViewModelProviderFactory, ViewModelProvider.Factory> viewModelProviderFactoryFunction;

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        DeviceTypeListViewModel viewModel = new AutoDisposeViewModelProvider(this, viewModelProviderFactoryFunction.apply(this)).get(DeviceTypeListViewModel.class);
        DeviceTypeListActivityBinding binding = DeviceTypeListActivityBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        DeviceTypeListAdapter adapter = new DeviceTypeListAdapter(this
                , viewModel.provideDeviceTypeImageResMap()
                , viewModel.provideDeviceTypeList());
        binding.list.setAdapter(adapter);
        binding.list.setOnItemClickListener((parent, view, position, id) -> {
            Intent intent = new Intent();
            intent.putExtra(KEY_DEVICE_TYPE, Objects.requireNonNull(adapter.getItem(position)).first);
            setResult(RESULT_OK, intent);
            finish();
        });
    }

}